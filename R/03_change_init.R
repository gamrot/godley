#' Change initial value of a variable
#'
#' @export
#'
#' @param model SFC model object
#' @param name string variable name
#' @param value numeric value that will replace existing initial value
#'
#' @return updated SFC model object with new variable initial value

change_init <- function(model,
                        name,
                        value) {
  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_string(name)
  checkmate::assert_numeric(value)
  # conditions
  if (!(name %in% model$variables$name)) {
    stop("There is no variable named ", name, " in the model")
  }

  # divide equations into rhs lhs (endogenous)
  eqs <- model$equations %>%
    dplyr::filter(hidden == FALSE)
  eqs <- eqs$equation
  equations_sep <- tibble::tibble(eqs) %>%
    tidyr::separate(.data$eqs, c("lhs", "rhs"), "=") %>%
    dplyr::mutate(
      lhs = stringr::str_squish(lhs),
      rhs = stringr::str_squish(rhs)
    )

  if (name %in% equations_sep$lhs) {
    stop(name, " is endogenous ", "
Changing initial value is invalid for endogenous variables")
  }

  model$variables$init[model$variables$name == name] <- value

  # prepared = FALSE if model was prepared
  if (!is.null(model$prepared)) {
    if (model$prepared[[1]]) {
      model$prepared[[1]] <- FALSE
    }
  }

  message("Changed ", name, " initial value to ", value)

  return(model)
}
