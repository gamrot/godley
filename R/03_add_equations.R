#' Adds an equation to the model
#'
#' @param model a SFC object
#' @param equation equation string in format: 'x = y + z + w + 2'
#' @param desc equation description, e.g. consumption flow
#' @param hidden logical, indicates if equation is hidden
#' @return an updated SFC object containing equations 
#'
#' @export

add_equation <- function(model ,
                        equation = '',
                        desc = '',
                        hidden = FALSE) {
  
  stopifnot(is.character(equation), is.character(desc), is.logical(hidden) )
  new_eq <- tibble::tibble('equation' = equation, 'desc' = desc,'hidden' = hidden)
  if(is.null(model$equations)){
    model$equations <- new_eq
  }else{
    model$equations <- tibble::add_row(model$equations, new_eq)
  }
  return(model)
}
