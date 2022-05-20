#' Simulate the scenario of a stock-flow consistent model
#' @param model SFC model
#' @param scenario Name of scenario want to simulate
#' @param max_iter Maximum iterations allowed per period.
#' @param periods Total number of rows (periods) in the model.
#' @param .hidden_tol Error tolerance to accept the equality of the hidden equation. Defaults to 0.1.
#' @param tol Tolerance accepted to determine convergence.
#' @param method The method to use to find a solution. Defaults to "Gauss".
#' @return Updated model containing scenario simulate result.
#' @export

simulate <- function( model, scenario = 'baseline', max_iter = 350,
                      periods = 100, hidden_tol = 0.1, tol = 1e-08,
                      method = 'Gauss'){

origin <- model[[scenario]]$initial_matrix
shock <- attr(origin, 'shock')
calls <- attr(model$prepared, 'calls')
m <- as.matrix(origin)
m <- matrix(m, nrow = periods, ncol = length(origin), byrow=TRUE ,
            dimnames = list(c(1:periods), names(origin)))

if(!is.null(shock)){
  shock[is.na(shock$end),]$end <- periods
  shock[is.na(shock$start),]$start <- 1
  m <- prepare_scenario_matrix(m,shock)
}



if(method == 'Gauss'){
  m <- run_gauss_seidel(m, calls, periods,max_iter, tol)
} else {
  m <- run_newton(m, calls, periods,max_iter, tol)
}


# Check if hidden is fulfilled  
h <- model$equations %>%
  filter(hidden == TRUE) %>%
  select("equation") %>%
  tidyr::separate(.data$equation, c('lhs', 'rhs'), '=')  %>%
  mutate(lhs = stringr::str_squish(lhs),
         rhs = stringr::str_squish(rhs))
hl <- h$lhs
hr <- h$rhs


if(all(abs(m[, hl] - m[, hr]) >= hidden_tol) & length(abs(m[, hl] - m[, hr])) >0  ){
  rlang::abort("Hidden equation is not fulfilled. Check the model try again. 
           If the problem persists, try `hidden = FALSE`or change the tolerance 
           level.")
} 
          

m<- tibble::tibble(data.frame(m))
#m["period"] <- 1:nrow(m)
m <- m[ , !grepl( "block" , colnames( m ) ) ]
model[[scenario]]$result <- m
return(model)

}













