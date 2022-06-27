#' Simulate scenario of SFC model object
#'
#' @export
#'
#' @param model SFC model object
#' @param scenario vector of strings or single string name of scenario(s) to simulate
#' @param max_iter numeric maximum iterations allowed per period, defaults to 350
#' @param periods numeric total number of rows (periods) in the model, defaults to 100
#' @param hidden_tol numeric error tolerance to accept the equality of hidden equations, defaults to 0.1.
#' @param tol numeric tolerance accepted to determine convergence, defaults to 1e-08
#' @param method string name of method used to find solution chosen from: 'Gauss', 'Newton', defaults to 'Gauss'
#'
#' @return updated model containing simulated scenario(s)

simulate_scenario <- function(model,
                              scenario,
                              max_iter = 350,
                              periods = 100,
                              hidden_tol = 0.1,
                              tol = 1e-08,
                              method = "Gauss") {

  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  if (!missing(scenario)) checkmate::assert_character(scenario)
  checkmate::assert_int(max_iter, lower = 0)
  checkmate::assert_int(periods, lower = 0)
  checkmate::assert_number(hidden_tol, lower = 0)
  checkmate::assert_number(tol, lower = 0)
  checkmate::assert_string(method)
  # conditions
  if (!(method %in% c("Gauss", "Newton"))) {
    stop(
      "There is no method named ", method,
      "Please choose from: Gauss, Newton"
    )
  }

  # prepare if unprepared
  if (is.null(model$prepared)) {
    model <- godley:::prepare(model)
  } else if (model$prepared[[1]] == F) {
    model <- godley:::prepare(model)
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

  message("Simulating ", no_scenarios, " scenario(s)")

  # simulating scenarios
  i <- 1
  for (scenario in scenarios_to_simulate) {
    message("Simulating scenario ", scenario, " (", i, " of ", no_scenarios, ")")
    origin <- model[[scenario]]$initial_matrix
    shock <- attr(origin, "shock")
    calls <- attr(model$prepared, "calls")
    m <- as.matrix(origin)
    m <- matrix(m,
      nrow = periods, ncol = length(origin), byrow = TRUE,
      dimnames = list(c(1:periods), names(origin))
    )

    if (!is.null(shock)) {
      shock[is.na(shock$end), ]$end <- periods
      shock[is.na(shock$start), ]$start <- 1
      m <- godley:::prepare_scenario_matrix(m, shock)
    }

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

    i <- i + 1
  }

  message("Scenario(s) successfully simulated")

  return(model)
}
