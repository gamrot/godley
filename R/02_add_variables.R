#' Adds variables to the model
#'
#' @param model a SFC object
#' @param name variable name
#' @param init initial value. Defaults to 1.00e-15.
#' @param desc variable description
#'
#' @return an updated SFC object containing variables 
#'
#' @export


add_variable <-function(model ,
                        name = '',
                        init =  0,
                        desc = '') {
  
  stopifnot(is.numeric(init), is.character(desc), is.character(name) )
  new_row <- tibble::tibble('name' = name, 'desc' = desc,'init' = init)
  if(is.null(model$variables)){
    model$variables <- new_row
  }else{
    model$variables <- tibble::add_row(model$variables, new_row)
  }
  return(model)
}
