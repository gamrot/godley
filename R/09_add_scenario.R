#' Add scenario to the model
#'
#' @export
#'
#' @param model SFC model object
#' @param name string name of scenario, defaults to 'expansion'
#' @param origin string name of origin scenario, from which the new scenario will be created, defaults to 'baseline'
#' @param origin_period numeric period number from origin scenario from which the new scenario will begin, defaults to 1
#' @param shock shock object from \code{create_shock()} and \code{add_shock()}
#'
#' @return updated SFC model object containing added scenario

add_scenario <- function(model,
                         name = "expansion",
                         origin = "baseline",
                         origin_period = 1,
                         shock) {

  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_string(name)
  checkmate::assert_string(origin)
  checkmate::assert_int(origin_period, lower = 0)

  if (!(is.null(model[[name]]$result))) {
    warning("Scenario named ", name, " already exists and will be overwritten")
  }

  if (is.null(model[[origin]]$result)) {
    stop("Origin scenario ", origin, " has not been simulated yet
Please simulate scenario before incorporating a shock")
  }

  initial_matrix <- model[[origin]]$result[origin_period, ]
  if (!is.null(initial_matrix$period)) {
    initial_matrix <- dplyr::select(initial_matrix, -period)
  }

  sh <- shock %>%
    dplyr::select("equation", "start", "end") %>%
    tidyr::separate(.data$equation, c("lhs", "rhs"), "=") %>%
    dplyr::mutate(
      lhs = stringr::str_squish(lhs),
      rhs = stringr::str_squish(rhs)
    )

  sc <- structure(initial_matrix,
    shock = sh
  )

  model[[name]]$initial_matrix <- sc

  return(model)
}
