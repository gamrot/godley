#' Add equations to the model
#'
#' @export
#'
#' @param model SFC model object
#' @param ... additional arguments passed to the function.
#'
#' @return updated SFC model object containing added equation

add_equation <- function(model, ...) {
  l <- list(...)
  t <- tibble::tibble(
    equation = character(),
    hidden = logical(),
    desc = character()
  )

  if (is.null(names(l))) names(l) <- rep("", length(l))

  for (i in 1:length(l)) {
    if ((checkmate::test_character(l[[i]]) | names(l[i]) == "equation") & names(l[i]) != "desc") {
      t <- rbind(t, c(equation = l[i], hidden = FALSE, desc = ""))
    }
    if (checkmate::test_logical(l[[i]]) | names(l[i]) == "hidden") {
      t$hidden[nrow(t)] <- l[i]
    }
    if (names(l[i]) == "desc") {
      t$desc[nrow(t)] <- l[i]
    }
  }

  for (i in 1:nrow(t)) {
    model <- add_equation_single(model, equation = t$equation[[i]], hidden = t$hidden[[i]], desc = t$desc[[i]])
  }

  return(model)
}

#' Add single equation to the model
#'
#' helper for add_equation()
#'
#' @param model SFC model object
#' @param equation string equation in format: 'x = y + z - a * b + (c + d) / e + f[-1]'
#' @param hidden logical, indicates if equation should be written as hidden, defaults to FALSE
#' @param desc string equation description
#'
add_equation_single <- function(model,
                                equation,
                                hidden = FALSE,
                                desc = "") {
  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_string(equation)
  checkmate::assert_flag(hidden)
  checkmate::assert_string(desc)

  new_eq <- tibble::tibble(equation = equation, hidden = hidden, desc = desc)
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

  return(model)
}
