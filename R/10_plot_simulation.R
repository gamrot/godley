#' Plot simulations of multiple variables in multiple scenarios
#'
#' @export
#'
#' @param model SFC model object
#' @param scenario vector of strings or single string name of scenario(s) from which take variables values, defaults to 'baseline'
#' @param take_all logical indicating whether all scenarios containing the given scenario name string(s) should be used, defaults to FALSE
#' @param from numeric period number from which the plot should start, defaults to maximum value
#' @param to numeric period number on which the plot should end, defaults to minimum value
#' @param expressions vector of strings or single string name of variable(s) expression(s) to plot, defaults to 'Y'
#'
#' @return \code{plotly} plot

plot_simulation <- function(model,
                            scenario = "baseline",
                            take_all = FALSE,
                            from = NA,
                            to = NA,
                            expressions = "Y") {
  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_character(scenario)
  checkmate::assert_flag(take_all)

  checkmate::assert(
    checkmate::check_int(from, na.ok = T, lower = 0),
    checkmate::check_string(from, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}"),
    checkmate::check_date(from)
  )
  if (
    checkmate::test_string(from, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}") |
      checkmate::test_date(from)
  ) {
    checkmate::assert_date(as.Date(from))
    from <- as.Date(from)
  }

  checkmate::assert(
    checkmate::check_int(to, na.ok = T, lower = 0),
    checkmate::check_string(to, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}"),
    checkmate::check_date(to)
  )
  if (
    checkmate::test_string(to, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}") |
      checkmate::test_date(to)
  ) {
    checkmate::assert_date(as.Date(to))
    to <- as.Date(to)
  }

  checkmate::assert_character(expressions)

  # conditions
  if (all(!is.na(c(from, to)))) {
    if (class(from) != class(to)) {
      stop("From and to are not of the same class, choose either numeric or Date")
    }
    if (from > to) {
      stop("From cannot be after to")
    }
  }

  if (take_all == TRUE) {
    scenarios <- c()
    for (i in scenario) {
      scenario_i <- names(model)[grepl(i, names(model))]
      scenario_i <- if (length(scenario_i) == 0) NULL else scenario_i
      scenarios <- append(scenarios, scenario_i)
    }

    scenario_pass <- scenario
    scenario <- scenarios

    if (is.null(scenario)) {
      stop(paste0("There is/are no scenario(s) named: ", paste0(scenario_pass, collapse = ", "), " in the model"))
    }
  }

  if (!(all(scenario %in% names(model)))) {
    stop(paste0("There is/are no scenario(s) named: ", paste0(scenario[!(scenario %in% names(model))], collapse = ", "), " in the model"))
  }

  results <- model$baseline$result %>% dplyr::select(time)


  for (i in scenario) {
    m <- model[[i]]$result

    exprs <- lapply(expressions, function(x) {
      gsub(.pvar(names(m)), "m\\[, '\\1'\\]", x, perl = T)
    })
    exprs <- purrr::map(exprs, function(x) parse(text = x))

    result_var <- m %>% dplyr::select(time)
    for (n in 1:length(expressions)) {
      result <- eval(exprs[[n]])
      name <- paste0(i, ": ", stringr::str_trim(stringr::str_split(expressions[[n]], "=")[[1]][1]))
      names(result) <- name
      result_var <- result_var %>% tibble::add_column(result)
    }

    results <- dplyr::full_join(results, result_var, by = "time")
  }

  if (is.na(from)) {
    from <- min(results$time)
  }
  if (is.na(to)) {
    to <- max(results$time)
  }

  results <- results %>% dplyr::filter(time >= from, time <= to)

  fig <- plotly::plot_ly()

  for (i in 2:ncol(results)) {
    legend <- paste(colnames(results)[i])

    fig <- plotly::add_trace(
      fig,
      y = results[[i]],
      x = results$time,
      name = legend,
      hovertemplate = paste("Time: %{x: 0f} <br> Value %{y:0f}"),
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 2),
      marker = list(size = 3)
    )
  }

  fig <- plotly::layout(
    fig,
    spikedistance = 1000,
    hovermode = "spikers",
    xaxis = list(title = "time"),
    showlegend = TRUE,
    legend = list(xanchor = "left", x = 1.02, y = 0.92)
  )

  if (length(scenario) == 1) {
    fig <- plotly::layout(
      fig,
      title = list(
        text = paste("Scenario", scenario),
        y = 0.95, x = 0.5,
        xanchor = "center", yanchor = "top"
      ),
      margin = list(t = 60, b = 70)
    )
  } else {
    fig <- plotly::layout(
      fig,
      title = list(
        text = "Multiple scenarios", y = 0.95, x = 0.5,
        xanchor = "center", yanchor = "top"
      ),
      margin = list(t = 60, b = 70)
    )
  }

  return(fig)
}
