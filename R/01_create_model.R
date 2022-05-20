#' SFC model constructor
#
#' @param name name for created SFC model
#'
#' @return SFC object  
#' @export 
#' 
create_model <- function(name = 'SFC model') {
  
  model<- structure(list(), class = 'SFC')
  model$name <- name
  return(model)
}


