#' Plot simulations of multiple variables in multiple scenarios
#'
#' @export
#'
#' @param model SFC model object
#' @param scenario vector of strings or single string name of scenario(s) from which take variables values, defaults to 'baseline'
#' @param take_all logical indicating whether all scenarios containing the given scenario name string(s) should be used, defaults to FALSE
#' @param from numeric period number from which the plot should start, defaults to 1
#' @param to numeric period number on which the plot should end, defaults to NA
#' @param expressions vector of strings or single string name of variable(s) expression(s) to plot, defaults to 'Y'
#'
#' @return \code{plotly} plot

plot_simulation <- function(model,
                            scenario = "baseline",
                            take_all = FALSE,
                            from = 1,
                            to = NA,
                            expressions = "Y") {

  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_character(scenario)
  checkmate::assert_flag(take_all)
  checkmate::assert_int(from, na.ok = T, lower = 0)
  checkmate::assert_int(to, na.ok = T, lower = 0)
  checkmate::assert_character(expressions)

  if (take_all == TRUE) {
    scenarios <- c()
    for (i in scenario) {
      scenario_i <- names(model)[grepl(i, names(model))]
      scenarios <- append(scenarios, scenario_i)
    }

    scenario_pass <- scenario
    scenario <- scenarios

    if (is.null(scenario)) {
      stop(paste(c("There is/ are no scenario(s) named: ", scenario_pass, "in the model"), collapse = ", "))
    }
  }

  if (!(all(scenario %in% names(model)))) {
    stop(paste(c("There is/ are no scenario(s) named: ", scenario[!(scenario %in% names(model))], "in the model"), collapse = ", "))
  }

  if (is.na(to)) {
    results <- tibble::tibble(periods = from:nrow(model[[scenario[1]]]$result))
  } else {
    results <- tibble::tibble(periods = from:to)
  }

  for (i in scenario) {
    m <- model[[i]]$result
    if (is.na(to)) {
      to <- nrow(m)
    }
    if (is.na(from)) {
      from <- 1
    }

    exprs <- lapply(expressions, function(x) {
      gsub(godley:::.pvar(names(m)), "m\\[, '\\1'\\]", x, perl = T)
    })
    exprs <- purrr::map(exprs, function(x) parse(text = x))

    result_var <- tibble::tibble(periods = 1:nrow(model[[scenario[1]]]$result))
    for (n in 1:length(expressions)) {
      result <- eval(exprs[[n]])
      names(result) <- paste0(i, ": ", stringr::str_trim(stringr::str_split(expressions[[n]], "=")[[1]][1]))
      result_var <- result_var %>% tibble::add_column(result)
    }
    result_var <- result_var[from:to, ]
    result_var <- result_var %>% dplyr::select(-c(periods))

    results <- results %>% tibble::add_column(result_var)
  }

  fig <- plotly::plot_ly()

  for (i in 2:ncol(results)) {
    legend <- paste(colnames(results)[i])

    fig <- plotly::add_trace(fig,
      y = results[[i]], x = results$periods,
      name = legend,
      hovertemplate = paste("Period: %{x: 0f} <br> Value %{y:0f}"),
      type = "scatter", mode = "lines+markers",
      line = list(width = 2),
      marker = list(size = 3)
    )
  }

  fig <- plotly::layout(fig,
    #    autosize = F,
    #    width = 1000,
    #    height = 500,
    spikedistance = 1000,
    hovermode = "spikers",
    xaxis = list(title = "period"),
    showlegend = TRUE,
    legend = list(xanchor = "left", x = 1.02, y = 0.92)
  )

  if (length(scenario) == 1) {
    fig <- plotly::layout(fig,
      title = list(text = paste("Scenario", scenario))
    )
  } else {
    fig <- plotly::layout(fig,
      title = list(text = "Multiple scenarios")
    )
  }

  return(fig)
}
