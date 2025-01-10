#' Calculate 1 order lag difference of a variable in model
#'
#' @export
#' @import tibble
#' @param x variable name
#'
#' @details this is a special function to be used exclusively in model equation strings e.g. "x = d(y) + z"
#'
#' @return difference

d <- function(x) {
  x_lag <- deparse(substitute(x), width.cutoff = 100)

  if (grepl("\\Qm[.i - 4,\\E", x_lag)) {
    x_lag <- stringr::str_replace_all(x_lag, "\\Q.i - 4\\E", ".i - 5")
  }

  if (grepl("\\Qm[.i - 3,\\E", x_lag)) {
    x_lag <- stringr::str_replace_all(x_lag, "\\Q.i - 3\\E", ".i - 4")
  }

  if (grepl("\\Qm[.i - 2,\\E", x_lag)) {
    x_lag <- stringr::str_replace_all(x_lag, "\\Q.i - 2\\E", ".i - 3")
  }

  if (grepl("\\Qm[.i - 1,\\E", x_lag)) {
    x_lag <- stringr::str_replace_all(x_lag, "\\Q.i - 1\\E", ".i - 2")
  }

  if (grepl("\\Qm[.i,\\E", x_lag)) {
    x_lag <- stringr::str_replace_all(x_lag, "\\Q.i\\E", ".i - 1")
  }

  return(x - eval(str2expression(x_lag), envir = parent.frame()))
}

#' Simulate scenario of SFC model object
#'
#' @export
#'
#' @param model SFC model object
#' @param scenario vector of strings or single string name of scenario(s) to simulate
#' @param max_iter numeric maximum iterations allowed per period, defaults to 350
#' @param periods numeric total number of rows (periods) in the model, defaults to 100
#' @param start_date character date to begin the simulation in the format "yyyy-mm-dd"
#' @param tol numeric tolerance accepted to determine convergence, defaults to 1e-05
#' @param hidden_tol numeric error tolerance to accept the equality of hidden equations, defaults to 0.1.
#' @param method string name of method used to find solution chosen from: 'Gauss', 'Newton', defaults to 'Gauss'
#' @param verbose logical to tell if additional model verbose should be displayed
#'
#' @return updated model containing simulated scenario(s)
#'

simulate_scenario <- function(model,
                              scenario,
                              periods = NA,
                              start_date = NA,
                              method = "Gauss",
                              max_iter = 350,
                              tol = 1e-05,
                              hidden_tol = 0.1,
                              verbose = FALSE) {
  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  if (!missing(scenario)) checkmate::assert_character(scenario)
  checkmate::assert_int(max_iter, lower = 0)
  checkmate::assert_int(periods, lower = 0, na.ok = T)
  checkmate::assert(
    checkmate::check_string(start_date, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}"),
    checkmate::check_date(start_date)
  )
  start_date <- as.Date(start_date)
  checkmate::assert_number(hidden_tol, lower = 0)
  checkmate::assert_number(tol, lower = 0)
  checkmate::assert_string(method)
  checkmate::assert_logical(verbose)
  # conditions
  if (!(method %in% c("Gauss", "Newton"))) {
    stop(
      "There is no method named ", method,
      "Please choose from: Gauss, Newton"
    )
  }

  # prepare if unprepared
  if (is.null(model$prepared)) {
    model <- prepare(model, verbose)
  } else if (model$prepared[[1]] == F) {
    model <- prepare(model, verbose)
  }

  # create list of all scenarios
  all_scenarios <- list()

  for (i in 1:length(names(model))) {
    if (!names(model)[i] %in% list("variables", "equations", "name", "prepared")) {
      all_scenarios <- append(all_scenarios, names(model)[i])
    }
  }

  # check if provided scenario name exists (if provided)
  if (!missing(scenario)) {
    for (i in scenario) {
      if (!i %in% all_scenarios) {
        stop("There is no scenario named ", i, " in the model")
      }
    }
  }

  # create list of not simulated scenarios to simulate based on provided scenarios or based on all scenarios
  scenarios_to_simulate <- list()

  if (!missing(scenario)) {
    for (i in scenario) {
      if (is.null(model[[i]]$result)) {
        scenarios_to_simulate <- append(scenarios_to_simulate, i)
      }
    }
  } else {
    for (i in all_scenarios) {
      if (is.null(model[[i]]$result)) {
        scenarios_to_simulate <- append(scenarios_to_simulate, i)
      }
    }
  }

  no_scenarios <- length(scenarios_to_simulate)

  # check if provided scenario was already calculated
  if (!missing(scenario) & (no_scenarios == 0)) {
    message("Provided scenario(s) is/ are already simulated")
    return(model)
  }

  # check if there is at least one scenario to simulate
  if (missing(scenario) & (no_scenarios == 0)) {
    message("All scenarios in this model are already simulated")
    return(model)
  }

  # simulating scenarios
  periods_user <- periods
  start_date_user <- start_date
  i <- 1
  for (scenario in scenarios_to_simulate) {
    message("Simulating scenario ", scenario, " (", i, " of ", no_scenarios, ")")

    periods <- periods_user
    start_date <- start_date_user

    if (!is.null(model[[scenario]]$shock)) {
      if (is.na(periods)) {
        periods <- nrow(model[[model[[scenario]]$origin]]$result)
      }
      if (is.na(start_date) &
        inherits(model[[model[[scenario]]$origin]]$result$time, "Date")) {
        start_date <- min(model[[model[[scenario]]$origin]]$result$time)
      }
      model <- prepare_scenario_matrix(model, scenario, periods)
    }

    if (is.na(periods)) periods <- 100

    origin <- model[[scenario]]$initial_matrix
    calls <- attr(model$prepared, "calls")
    m <- origin
    m_len <- dim(m)[1]

    if (m_len > periods) {
      m <- m[1:periods, ]
    } else if (m_len < periods) {
      m <- rbind(
        m,
        matrix(rep(m[m_len, ], periods - m_len), nrow = periods - m_len, byrow = T)
      )
    }

    dimnames(m) <- list(c(1:periods), colnames(origin))

    if (method == "Gauss") {
      m <- run_gauss_seidel(m, calls, periods, max_iter, tol, verbose)
    } else if (method == "Newton") {
      m <- run_newton(m, calls, periods, max_iter, tol)
    }

    # Check if hidden is fulfilled
    h <- model$equations %>%
      dplyr::filter(hidden == TRUE) %>%
      dplyr::select("equation") %>%
      tidyr::separate(.data$equation, c("lhs", "rhs"), "=") %>%
      dplyr::mutate(
        lhs = stringr::str_squish(lhs),
        rhs = stringr::str_squish(rhs)
      )
    hl <- h$lhs
    hr <- h$rhs

    # Check if hidden equations are fulfilled
    diffs <- m[, hl, drop = FALSE] - m[, hr, drop = FALSE]

    # Identify any hidden equations that fail the tolerance criterion
    failing_equations <- which(apply(abs(diffs), 2, max) >= hidden_tol)

    if (length(failing_equations) > 0) {
      # Construct a detailed message for each failing equation
      eq_messages <- sapply(failing_equations, function(i) {
        eq_lhs <- h$lhs[i]
        eq_rhs <- h$rhs[i]
        max_diff <- max(abs(diffs[, i]))
        paste0(
          "Hidden equation '", eq_lhs, " = ", eq_rhs,
          "' does not hold within the hidden tolerance of ", hidden_tol,
          ". Maximum difference observed: ", max_diff
        )
      })

      error_message <- paste0(
        "\nThe following hidden (redundant) equation(s) are not fulfilled:\n",
        paste(eq_messages, collapse = "\n"),
        "\n\nHidden equations serve as a critical check on the model's water tight accounting,",
        "which is one of the defining aspects of a SFC model.",
        " In a properly specified and converged stationary SFC model,",
        " these redundant conditions should be met exactly (within the chosen tolerance).",
        "\n\nIf these conditions fail, it may indicate an issue with the model's internal consistency or equilibrium conditions.",
        " Possible steps to address this issue include:\n",
        " - Double-checking the model's specification and variables values.\n",
        " - Adjusting the `hidden_tol` to a higher value if you believe the differences are negligible.\n",
        " - Changing the solution method (e.g., from 'Gauss' to 'Newton') to improve convergence.\n",
        " - Trying `hidden = FALSE` to ignore these redundant checks if appropriate.\n"
      )
      if (!verbose) {
        error_message <- paste0(
          error_message,
          "\nInclude the simulate_scenario(..., verbose = TRUE) parameter to get more details on the root causes of issues during execution."
        )
      }
      stop(error_message)
    }

    m <- tibble::tibble(data.frame(m))
    m <- m[, !grepl("block", colnames(m))]
    model[[scenario]]$result <- m

    # add time
    if (!is.na(start_date)) {
      model[[scenario]]$result <- tibble::add_column(
        tibble::tibble(time = seq(as.Date(start_date), by = "quarter", length.out = periods)),
        model[[scenario]]$result
      )
    } else {
      model[[scenario]]$result <- tibble::add_column(
        tibble::tibble(time = as.numeric(c(1:periods))),
        model[[scenario]]$result
      )
    }

    i <- i + 1
  }

  message("Scenario(s) successfully simulated")

  return(model)
}
