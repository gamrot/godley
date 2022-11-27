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

.pvarlag_1 <- function(x) {
  paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?=\\[-1\\])")
}

# ' Pattern replacement lag 2
# '
# ' @param x vector of variables

.pvarlag_2 <- function(x) {
  paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?=\\[-2\\])")
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
# ' @param equations_sep tibble of equations after passing \code{validate_model_input()} function
# ' @param variables_exo tibble of exogenous values after passing \code{validate_model_input()} function
# '
# ' @return tibble of equations with the correct matrix syntax

prep_equations <- function(equations_sep,
                           variables_exo) {
  x <- equations_sep %>%
    dplyr::mutate(
      rhs = gsub(godley:::.pvar(equations_sep$lhs),
        "m\\[.i, '\\1'\\]", .data$rhs,
        perl = T
      ),
      rhs = gsub(godley:::.pvarlag_1(equations_sep$lhs),
        "m\\[.i-1,'\\1'\\]", .data$rhs,
        perl = T
      ),
      rhs = gsub(godley:::.pvarlag_2(equations_sep$lhs),
        "m\\[.i-2,'\\1'\\]", .data$rhs,
        perl = T
      )
    )

  if (!rlang::is_empty(variables_exo$lhs)) {
    x <- x %>%
      dplyr::mutate(
        rhs = gsub(godley:::.pvar(variables_exo$lhs),
          "m\\[.i, '\\1'\\]", .data$rhs,
          perl = T
        ),
        rhs = gsub(godley:::.pvarlag_1(variables_exo$lhs),
          "m\\[.i-1,'\\1'\\]", .data$rhs,
          perl = T
        ),
        rhs = gsub(godley:::.pvarlag_2(variables_exo$lhs),
          "m\\[.i-2,'\\1'\\]", .data$rhs,
          perl = T
        )
      )
  }

  x <- x %>%
    dplyr::mutate(
      rhs = gsub("\\[-1\\]", "", .data$rhs),
      rhs = gsub("\\[-2\\]", "", .data$rhs),
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
  equations_sep <- res[[1]]
  variables_exo <- res[[2]]
  functions <- res[[3]]

  # Prepare them for the simulation process
  km <- godley:::find_adjacency(equations_sep)
  blocks <- godley:::find_blocks(km)
  equations_sep$block <- blocks
  calls <- godley:::prep_equations(equations_sep, variables_exo)

  if (length(functions) != 0) {
    for (fun in functions) {
      patt <- paste0("m\\[.i, \\'", fun, "\\'\\]\\(")
      repl <- paste0(fun, "(")
      calls$rhs <- stringr::str_replace_all(calls$rhs, patt, repl)
    }
  }

  var <- model$variables$init
  names(var) <- model$variables$name
  all_var <- c(calls$lhs, variables_exo$lhs)
  var <- var[all_var]
  names(var) <- all_var

  if (any(is.na(var))) var[is.na(var)] <- 1e-10 # default init value

  m1 <- tibble::tibble(as.data.frame(t(x = var)))
  prepared <- structure(tibble::tibble(prepared = T),
    calls = calls
  )
  model$prepared <- prepared
  model$baseline$initial_matrix <- m1

  message("Model prepared successfully")

  return(model)
}
