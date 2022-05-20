#' Make initial matrix  row for scenario and prepare shocks
#'
#' @param model a SFC model
#' @param name scenario name
#' @param origin origin scenario which from want to simulate 
#' @param origin_period origin period in origin scenario which from want to simulate.
#' Defaults 1. 
#' @param shocks tibble making using \code{create_shock()} and \code{add_shock()}
#' @export 
add_scenario <- function(model, name = 'expansion', origin = 'baseline',
                         origin_period =1 , shocks){
  
 initial_matrix <- model[[origin]]$result[origin_period,]
 
 sh <- shocks %>%
   select('equation', 'start', 'end') %>%
   tidyr::separate(.data$equation, c('lhs', 'rhs'), '=') %>%
   mutate(lhs = stringr::str_squish(lhs),
          rhs = stringr::str_squish(rhs))
 
 sc <- structure(initial_matrix,
                 shock = sh)
 
 model[[name]]$initial_matrix <- sc
 
 
 return(model)
 
}

