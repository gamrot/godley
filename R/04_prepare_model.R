#' Find dependencies and order the equations
#'
#' @param x A vector to modify
#' @export
#'

add_lag_info <- function(x) {
  gsub("\\[-1\\]", "__", x)
}

#' Find adjacency matrix for a system of equations
#'
#' @param equations A system of equations already time stamped
#'
#' @author João Macalós
#' @return Adjacency matrix
#' @export
#'
sfcr_find_adjacency <- function(equations) {
  km <- matrix(0L, nrow = length(equations$lhs), ncol = length(equations$lhs))
  rownames(km) <- equations$lhs
  colnames(km) <- equations$lhs
  k <- equations %>%
    dplyr::mutate(rhs = stringr::str_extract_all(.data$rhs, .pvar(equations$lhs)))
  for (var in seq_along(k$lhs)) {
    km[k$lhs[[var]], k$rhs[[var]]] <- 1
  }
  return(km)
}

#' Pattern replacement var
#' @param x vector of variables
#'
#' @author João Macalós
#'
#' @export
#' 
.pvar <- function(x) {paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")}

#' Pattern replacement lag
#' @param x vector of variables
#'
#' @author João Macalós
#' @export

.pvarlag <- function(x) {paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?=__)")}


#' Find blocks of independent equations (using \code{igraph} functions)
#'
#' @param adj Adjacency matrix
#'
#' @author João Macalós
#'
#' @export
find_blocks <- function(adj) {
  g <- igraph::graph.adjacency(adjmatrix = t(adj),mode = "directed")
  blocks <- igraph::components(g, "strong")$membership
  return(blocks)
}

#' Re-wrote the equations with the correct matrix syntax that will be used to evaluate
#' the expressions inside the Gauss Seidel/Newton algorithm
#'
#' @param eqs_separated Tibble of equations after passing  \code{validate_data()} function.
#' @param external_values Tibble of exogenous values after passing  \code{validate_data()} function.
#' @return Tibble of equations with the correct matrix syntax
#' @export
prep_equations <- function(eqs_separated, external_values) {
  x <- eqs_separated %>%
    dplyr::mutate(rhs = gsub(.pvar(eqs_separated$lhs), 
                             "m\\[.i, '\\1'\\]", .data$rhs, perl = T),
                  rhs = gsub(.pvarlag(eqs_separated$lhs), 
                             "m\\[.i-1,'\\1'\\]", .data$rhs, perl = T),
                  rhs = gsub(.pvar(external_values$lhs), 
                             "m\\[.i,'\\1'\\]", .data$rhs, perl = T),
                  rhs = gsub(.pvarlag(external_values$lhs), 
                             "m\\[.i-1,'\\1'\\]", .data$rhs, perl = T),
                  rhs = gsub("__", "", .data$rhs),
                  id = dplyr::row_number())
  return(x)
} 

#' Making initial matrix row for baseline scenario and prepare equations for \code{simulate()} 

#' @param model a SFC object
#'
#' @return indicator that model is prepared 
#'
#' @export

prepare <- function(model){
 
# ---- Check correctness of equations entered by the user
  
res <- validate_data(model) 
eqs_separated <- res[[1]]
external_values  <- res[[2]]

# ---- Prepare them for the simulation process

km <- sfcr_find_adjacency(eqs_separated)
blocks <- find_blocks(km)
eqs_separated$block <- blocks
calls <- prep_equations(eqs_separated, external_values)


var <- model$variables$init
names(var) <- model$variables$name
all_var <- c(calls$lhs, external_values$lhs)
var <- var[all_var]
names(var) <- all_var

if(any(is.na(var))){
  d <- names(var[which(is.na(var))])
  var[is.na(var)] <- 0
  warning(message = paste(c("Variable(s): ", d, " take(s) default 0 value."), collapse= " "))
}

#blocks <- paste0("block", sort(unique(calls$block)))
#lblocks <- rep(0, length(blocks))
#names(lblocks) <- blocks
#m1 <- c(var, lblocks)
m1 <- tibble::tibble(as.data.frame(t(x = var)))
prepared <- structure(tibble(prepared = T), 
                      calls = calls)
model$prepared <- prepared
model$baseline$initial_matrix <- m1
return(model)

}
