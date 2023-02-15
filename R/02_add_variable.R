#' Add variables to the model
#'
#' @export
#'
#' @param model SFC model object
#' @param ... additional arguments passed to the function.
#'
#' @return updated SFC model object containing added variable

add_variable <- function(model, ...) {
  l <- list(...)
  t <- tibble::tibble(
    name = character(),
    init = numeric(),
    desc = character()
  )

  if (is.null(names(l))) names(l) <- rep("", length(l))

  for (i in 1:length(l)) {
    if ((checkmate::test_character(l[[i]]) | names(l[i]) == "name") & names(l[i]) != "desc") {
      t <- rbind(t, c(name = l[i], init = NA, desc = ""))
    }
    if (checkmate::test_numeric(l[[i]]) | names(l[i]) == "init") {
      t$init[nrow(t)] <- l[i]
    }
    if (names(l[i]) == "desc") {
      t$desc[nrow(t)] <- l[i]
    }
  }

  for (i in 1:nrow(t)) {
    model <- add_variable_single(model, name = t$name[[i]], init = t$init[[i]], desc = t$desc[[i]])
  }

  return(model)
}

#' Add single variable to the model
#'
#' helper for add_variable()
#'
#' @param model SFC model object
#' @param name string name for added variable
#' @param init numeric initial value, defaults to 1e-05
#' @param desc string variable description
#'

add_variable_single <- function(model,
                                name,
                                init = NA,
                                desc = "") {
  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_string(name)
  checkmate::assert_numeric(init)
  checkmate::assert_string(desc)

  new_row <- tibble::tibble(name = name, init = list(init), desc = desc)
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

  return(model)
}
