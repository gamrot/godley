#' @useDynLib godley, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
NULL

# Declare global variables to avoid "no visible binding" notes during checks
utils::globalVariables(c(
  ".", ".data", "time", "hidden", "lhs", "rhs", "name", "init", "equation", ":=", "block"
))
