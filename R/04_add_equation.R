#' Add equation to the model
#'
#' @export
#'
#' @param model SFC model object
#' @param equation string equation in format: 'x = y + z - a * b + (c + d) / e + f[-1]'
#' @param desc string equation description
#' @param hidden logical, indicates if equation should be written as hidden, defaults to FALSE
#'
#' @return updated SFC model object containing added equation

add_equation <- function(model,
                         equation,
                         desc = "",
                         hidden = FALSE) {

  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_string(equation)
  checkmate::assert_string(desc)
  checkmate::assert_flag(hidden)

  new_eq <- tibble::tibble("equation" = equation, "desc" = desc, "hidden" = hidden)
  if (is.null(model$equations)) {
    model$equations <- new_eq
  } else {
    model$equations <- tibble::add_row(model$equations, new_eq)
  }

  # prepared = FALSE if model was prepared
  if (!is.null(model$prepared)) {
    if (model$prepared[[1]]) {
      model$prepared[[1]] <- FALSE
    }
  }

  message("Added equation ", equation)

  return(model)
}
