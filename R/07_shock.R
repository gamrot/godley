#' Constructor shock tibble used in \code{add_scenario()}.
#' @return Tibble object  
#' @export 
#' 
create_shock <- function() {
  shock<- tibble::tibble('equation' = character(), 'desc' = character(),
                         'start' = numeric(), 'end' = numeric())
  return(shock)
}

#' Define chenge of exogenous variable and time for shocking selected scenario
#'
#' @param shock a tibble object after \code{create_shock}
#' @param equation equation  string in format: 'x = 2'
#' @param desc variable description
#' @param start Defaults NA.
#' @param end Defaults NA.
#' @return an updated shock object containing information 
#'
#' @export


add_shock <-function(shock,
                        equation = '',
                        desc = '',
                        start = NA,
                        end = NA
                        ){
  
  stopifnot(any(is.na(start),is.numeric(start)),
            any(is.na(end),is.numeric(end)),
            is.character(desc), is.character(equation))
  new_row <- tibble::tibble('equation' = equation, 'desc' = desc,'start' = start,
                            'end' = end)
  shock <- tibble::add_row(shock, new_row)
  return(shock)
}

