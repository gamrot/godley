#' Create model with sensitivity scenarios
#'
#' @export
#'
#' @param model_pass SFC model object that will be used as a baseline for sensitivity calculation
#' @param variable string name of the variable that will be used
#' @param lower numeric lower bound value of the variable
#' @param upper numeric upper bound value of the variable
#' @param step numeric step between upper and lower bounds for the variable to take value
#'
#' @return SFC model object with sensitivity scenarios

create_sensitivity <- function(model_pass,
                               variable,
                               lower = 0,
                               upper = 1,
                               step = 0.1) {

  # arguments check
  # type
  checkmate::assert_class(model_pass, "SFC")
  checkmate::assert_string(variable)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_number(step)

  # create new model as a copy of provided model
  model <- create_model(name = paste("Sensitivity", variable, sep = "_"), template = model_pass)
  model <- godley:::prepare(model)

  # check if provided variable is exogenous
  ## split equations into two columns
  eqs <- model$equations %>%
    dplyr::filter(hidden == FALSE)
  eqs <- eqs$equation
  eqs_separated <- tibble::tibble(eqs) %>%
    tidyr::separate(.data$eqs, c("lhs", "rhs"), "=") %>%
    dplyr::mutate(
      lhs = stringr::str_squish(lhs),
      rhs = stringr::str_squish(rhs)
    )
  eqs_separated <- eqs_separated %>%
    dplyr::mutate(rhs = godley:::add_lag_info(.data$rhs))

  ## extract exogenous
  external_values <- setdiff(model$variables$name, eqs_separated$lhs)
  external_values <- model$variables %>%
    dplyr::filter(name %in% external_values) %>%
    dplyr::select(c("name", "init")) %>%
    dplyr::rename(
      lhs = name,
      rhs = init
    )

  ## check if provided variable is exogenous
  if ((variable %in% external_values$lhs == F) & (variable %in% eqs_separated$lhs == F)) {
    stop("There is no variable named ", variable, " in the model")
  } else if (variable %in% external_values$lhs == F) {
    stop(variable, " is endogenous ", "
Sensitivity calculation is invalid for endogenous variables")
  }

  # for each variable value create new scenario
  for (i in seq(lower, upper, by = step)) {
    # create temp initial matrix
    tmp <- model$baseline
    # change variable value
    tmp$initial_matrix[variable] <- i
    # save initial matrix with new variable value as new scenario
    model[paste("sensitivity", variable, i, sep = "_")] <- list(tmp)
  }

  return(model)
}
