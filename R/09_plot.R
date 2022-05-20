#' Plotting series comparison of a given model' scenario.
#'
#' Maximum number of series is 5 (to properly choose color from a vector)
#'
#' @param model a SFC model object, used mostly for enriching a plot
#' @param scenario a name of scenario from which take variables values
#' @param expressions a vector (or single character string) containing information
#'   which variable/s expressions to plot
#' @param from first iteration to plot
#' @param to last iteration to plot
#' @return plotly plot object ready to show / save
#' @export

plot_series <- function(model , scenario = 'baseline',from = 1, to = NA,
                        expressions = list('Y')){
  
  colors = c('rgba(152, 0, 0, .8)', 'rgb(0, 0, 255)','rgb(255, 171, 0)',
             'rgb(255, 118, 255)', 'rgb(0, 0, 47)', 'rgb(255, 0, 0)')
  m <- model[[scenario]]$result
  if(is.na(to)){
    to = nrow(m)
  }
  if(is.na(from)){
    from = 1
  }
  exprs <- lapply(expressions, function(x){gsub(.pvar(names(m)), "m\\[, '\\1'\\]", x, perl = T)})


  exprs <- purrr::map(exprs, function(x) parse(text=x))
  
  results <- tibble(periods = 1:nrow(m))
  for (i in 1:length(expressions)){
    result <- eval(exprs[[i]])
    names(result) <- expressions[[i]]
    results <- results %>% 
      tibble::add_column(result)
  }
  
  results <- results[from:to,]
  
  fig <- plotly::plot_ly (y = results[[2]], x = results$periods,
                   name = paste("function: ", colnames(results)[2]) ,
                   hovertemplate = paste("Period: %{x: 0f} <br> Value %{y:0f}"),
                   type = 'scatter', mode = 'lines+markers',
                   line = list(color = colors[1], width = 2),
                   marker = list(color = colors[1], size = 3))
  
  if(length(expressions) >1){
  for(i in 3:ncol(results)){
    legend = paste("function: ", colnames(results)[i])
    color = colors[i]
    
    fig <-plotly::add_trace(fig, y = results[[i]], x = results$periods,
                   name = legend ,
                   hovertemplate = paste("Period: %{x: 0f} <br> Value %{y:0f}"),
                   type = 'scatter', mode = 'lines+markers',
                   line = list(color = color, width = 2),
                   marker = list(color = color, size = 3))
  }}
  
fig <- plotly::layout(fig, spikedistance = 1000,
              hovermode = 'spikers',
              xaxis = list( title = 'period'),
              title = list( text = paste("Scenario ", scenario)))


return(fig)
  
}