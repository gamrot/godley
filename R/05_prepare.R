# ' Find dependencies and order equations
# '
# ' @param x vector to modify

add_lag_info <- function(x) {
  gsub("\\[-1\\]", "__", x)
}

# ' Find adjacency matrix for system of equations
# '
# ' @author João Macalós
# '
# ' @param equations system of equations already time stamped
# '
# ' @return adjacency matrix

find_adjacency <- function(equations) {
  km <- matrix(0L, nrow = length(equations$lhs), ncol = length(equations$lhs))
  rownames(km) <- equations$lhs
  colnames(km) <- equations$lhs
  k <- equations %>%
    dplyr::mutate(rhs = stringr::str_extract_all(.data$rhs, godley:::.pvar(equations$lhs)))
  for (var in seq_along(k$lhs)) {
    km[k$lhs[[var]], k$rhs[[var]]] <- 1
  }
  return(km)
}

# ' Pattern replacement var
# '
# ' @author João Macalós
# '
# ' @param x vector of variables

.pvar <- function(x) {
  paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")
}

# ' Pattern replacement lag
# '
# ' @author João Macalós
# '
# ' @param x vector of variables

.pvarlag <- function(x) {
  paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?=__)")
}

# ' Find blocks of independent equations (using \code{igraph} functions)
# '
# ' @author João Macalós
# '
# ' @param adj adjacency matrix

find_blocks <- function(adj) {
  g <- igraph::graph.adjacency(adjmatrix = t(adj), mode = "directed")
  blocks <- igraph::components(g, "strong")$membership
  return(blocks)
}

# ' Re-write the equations with the correct matrix syntax that will be used to evaluate the expressions inside the Gauss Seidel/Newton algorithm
# '
# ' @param eqs_separated tibble of equations after passing \code{validate_model_input()} function
# ' @param external_values tibble of exogenous values after passing \code{validate_model_input()} function
# '
# ' @return tibble of equations with the correct matrix syntax

prep_equations <- function(eqs_separated,
                           external_values) {
  x <- eqs_separated %>%
    dplyr::mutate(
      rhs = gsub(godley:::.pvar(eqs_separated$lhs),
        "m\\[.i, '\\1'\\]", .data$rhs,
        perl = T
      ),
      rhs = gsub(godley:::.pvarlag(eqs_separated$lhs),
        "m\\[.i-1,'\\1'\\]", .data$rhs,
        perl = T
      )
    )

  if (!rlang::is_empty(external_values$lhs)) {
    x <- x %>%
      dplyr::mutate(
        rhs = gsub(godley:::.pvar(external_values$lhs),
          "m\\[.i,'\\1'\\]", .data$rhs,
          perl = T
        ),
        rhs = gsub(godley:::.pvarlag(external_values$lhs),
          "m\\[.i-1,'\\1'\\]", .data$rhs,
          perl = T
        )
      )
  }

  x <- x %>%
    dplyr::mutate(
      rhs = gsub("__", "", .data$rhs),
      id = dplyr::row_number()
    )

  return(x)
}

#' Make initial matrix row for baseline scenario and prepare equations for \code{simulate_scenario()}
#'
#' @export
#'
#' @param model SFC model object
#'
#' @return verified and prepared SFC model object

prepare <- function(model) {

  # argument check
  # type
  checkmate::assert_class(model, "SFC")

  # Check correctness of equations entered by the user
  res <- godley:::validate_model_input(model)
  eqs_separated <- res[[1]]
  external_values <- res[[2]]
  funs <- res[[3]]

  # Prepare them for the simulation process
  km <- godley:::find_adjacency(eqs_separated)
  blocks <- godley:::find_blocks(km)
  eqs_separated$block <- blocks
  calls <- godley:::prep_equations(eqs_separated, external_values)

  if (length(funs) != 0) {
    for (fun in funs) {
      patt <- paste0("m\\[.i, \\'", fun, "\\'\\]\\(")
      repl <- paste0(fun, "(")
      calls$rhs <- stringr::str_replace_all(calls$rhs, patt, repl)
    }
  }

  var <- model$variables$init
  names(var) <- model$variables$name
  all_var <- c(calls$lhs, external_values$lhs)
  var <- var[all_var]
  names(var) <- all_var

  if (any(is.na(var))) {
    d <- names(var[which(is.na(var))])
    var[is.na(var)] <- 0
    message(paste(c("Variable(s):", d, "take(s) default 0 value"), collapse = " "))
  }

  m1 <- tibble::tibble(as.data.frame(t(x = var)))
  prepared <- structure(tibble::tibble(prepared = T),
    calls = calls
  )
  model$prepared <- prepared
  model$baseline$initial_matrix <- m1

  message("Model prepared successfully")

  return(model)
}
