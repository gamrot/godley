#' Calculate 1 order lag difference of a variable in model
#'
#' @export
#'
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
#' @param info logical to tell if additional model info should be displayed
#'
#' @return updated model containing simulated scenario(s)

simulate_scenario <- function(model,
                              scenario,
                              periods = NA,
                              start_date = NA,
                              method = "Gauss",
                              max_iter = 350,
                              tol = 1e-05,
                              hidden_tol = 0.1,
                              info = FALSE) {
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
  checkmate::assert_logical(info)
  # conditions
  if (!(method %in% c("Gauss", "Newton"))) {
    stop(
      "There is no method named ", method,
      "Please choose from: Gauss, Newton"
    )
  }

  # prepare if unprepared
  if (is.null(model$prepared)) {
    model <- godley:::prepare(model, info)
  } else if (model$prepared[[1]] == F) {
    model <- godley:::prepare(model, info)
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
        class(model[[model[[scenario]]$origin]]$result$time) == "Date") {
        start_date <- min(model[[model[[scenario]]$origin]]$result$time)
      }
      model <- godley:::prepare_scenario_matrix(model, scenario, periods)
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
      m <- godley:::run_gauss_seidel(m, calls, periods, max_iter, tol)
    } else if (method == "Newton") {
      m <- godley:::run_newton(m, calls, periods, max_iter, tol)
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

    if (any(abs(m[, hl] - m[, hr]) >= hidden_tol) & length(abs(m[, hl] - m[, hr])) > 0) {
      stop("Hidden equation is not fulfilled
Plesae check the model try again
If the problem persists, try `hidden = FALSE` or change the tolerance level")
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
