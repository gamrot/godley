#' 'Validates' a model (checks conditions and verify information introduced by user )
#' 
#' Checking: initial conditions, defined / not defined variables existence
#'
#' @param model a SFC object
#'
#' @return an updated equation and external values
#'
#' @export
validate_data <- function(model){
  #1
  if(is.null(model$variables)){ 
    stop(message = 'Section model$variables is empty. 
         Please complete exogenous variables and initial values.') 
    }
  #2
  if(is.null(model$equations)){ 
    stop(message = 'Section model$equations is empty. 
         Please complete equations.')
    }
  #3 
  equations<- model$equations$equation
  is_invalid_name <- stringr::str_detect(equations,'[!?,~:;#``@$]') #\\'[!?><,~:;#^``@$%}{|]')
  if (sum(is_invalid_name)!=0) {
    stop('Invalid sign in equations.')
  }
  #4
  var <- model$variables$name
  if(sum(duplicated(var)) != 0 ){
    stop('Variable declared more than once. Check $variables')
  }

  # divide equations into rhs lhs (endogenous)
  eqs <- model$equations %>% 
    filter(hidden == FALSE)
  eqs <- eqs$equation
  eqs_separated <- tibble::tibble(eqs) %>%
    tidyr::separate(.data$eqs, c('lhs', 'rhs'), '=')  %>%
    mutate(lhs = stringr ::str_squish(lhs),
           rhs = stringr ::str_squish(rhs))
  eqs_separated <- eqs_separated %>%
    dplyr::mutate(rhs = add_lag_info(.data$rhs))
  
  # extract exogenous
  external_values <- setdiff(model$variables$name ,eqs_separated$lhs)
  external_values <- model$variables %>%
    filter(name %in% external_values) %>%
    select(c("name", "init")) %>%
    rename(lhs = name,
           rhs = init)
  
  if(sum(external_values$rhs == 0)){
    d <- external_values[which(external_values$rhs == 0),]$lhs
    warning(message = paste(c("Exogenous variable(s): ", d, " take(s) default 0 value."), collapse= " "))
    
  }
  
  return(list(eqs_separated, external_values))
  
  
}



