#' Making initial matrix row for scenario specified by \code{create_shock()} and 
#' \code{add_scenario()}. 

#' @param model a SFC object
#' @param shock Tibble created by \code{create_shock()}
#' @return Matrix with initial matrix ready to \code{simulate()}
#'
#' @export


prepare_scenario_matrix <- function(m ,shock){
  
  for(i in seq(nrow(shock)) ){
    
   m[shock[i,]$start:shock[i,]$end, shock[i,]$lhs ] <- as.numeric(shock[i,]$rhs)
  }
  return(m)
}
