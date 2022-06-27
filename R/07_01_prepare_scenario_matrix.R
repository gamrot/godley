# ' Make initial matrix row for scenario specified by \code{create_shock()} and \code{add_scenario()}.
# '
# ' @param m SFC model object
# ' @param shock tibble created by \code{create_shock()}
# '
# ' @return initial matrix ready to \code{simulate_scenario()}

prepare_scenario_matrix <- function(m,
                                    shock) {
  for (i in seq(nrow(shock))) {
    m[shock[i, ]$start:shock[i, ]$end, shock[i, ]$lhs] <- as.numeric(shock[i, ]$rhs)
  }
  return(m)
}
