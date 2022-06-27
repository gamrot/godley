#' Add variable to the model
#'
#' @export
#'
#' @param model SFC model object
#' @param name string name for added variable
#' @param init numeric initial value, defaults to 1e-15
#' @param desc string variable description
#'
#' @return updated SFC model object containing added variable

add_variable <- function(model,
                         name,
                         init = 0,
                         desc = "") {

  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_string(name)
  checkmate::assert_number(init)
  checkmate::assert_string(desc)

  new_row <- tibble::tibble("name" = name, "desc" = desc, "init" = init)
  if (is.null(model$variables)) {
    model$variables <- new_row
  } else {
    model$variables <- tibble::add_row(model$variables, new_row)
  }

  # prepared = FALSE if model was prepared
  if (!is.null(model$prepared)) {
    if (model$prepared[[1]]) {
      model$prepared[[1]] <- FALSE
    }
  }

  message("Added variable ", name, ", with initial value ", init)

  return(model)
}
