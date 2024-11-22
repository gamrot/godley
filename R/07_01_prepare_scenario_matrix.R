# ' Make initial matrix row for scenario specified by \code{create_shock()} and \code{add_scenario()}.

#' @importFrom data.table :=

prepare_scenario_matrix <- function(model, scenario, periods) {
  initial_matrix <- model[[scenario]]$initial_matrix
  shock <- model[[scenario]]$shock
  time_class <- attr(shock, "time_class_shock")

  # create initial matrix from origin
  m_len <- nrow(initial_matrix)

  if (m_len < periods) {
    if (time_class == "Date") {
      initial_times <- initial_matrix$time
    }
    initial_matrix <- dplyr::select(initial_matrix, -time)
    initial_matrix <- tibble::add_row(
      initial_matrix,
      tibble::tibble(initial_matrix[rep(m_len, periods - m_len), ])
    )
    if (time_class == "numeric") {
      initial_matrix <- tibble::add_column(
        time = c(1:periods),
        initial_matrix
      )
    } else if (time_class == "Date") {
      initial_matrix <- tibble::add_column(
        time = c(
          initial_times,
          seq(as.Date(tail(initial_times, 1)), by = "quarter", length.out = periods - m_len)
        ),
        initial_matrix
      )
    }
  }

  # create shock data frame
  for (s in names(shock)) {
    shock_start <- shock[[s]]$times$start
    shock_end <- shock[[s]]$times$end
    shock_name <- paste0(s, "_shock")

    if (time_class == "numeric") {
      if (is.na(shock_start)) shock_start <- 1
      if (is.na(shock_end)) shock_end <- nrow(initial_matrix)

      times <- initial_matrix$time[c(shock_start:shock_end)]

      shock_type <- names(shock[[s]]$values)

      if (shock_type == "value") {
        values <- shock[[s]]$values$value
      } else if (shock_type == "rate") {
        values <- initial_matrix[, s][[1]][c(shock_start:shock_end)] *
          (1 + shock[[s]]$values$rate)
      } else if (shock_type == "absolute") {
        values <- initial_matrix[, s][[1]][c(shock_start:shock_end)] +
          shock[[s]]$values$absolute
      }

      if (length(values) > length(times)) {
        values <- values[1:length(times)]
      } else if (length(values) < length(times)) {
        values <- c(values, rep(tail(values, 1), length(times) - length(values)))
      }

      shock_tbl_s <- tibble::tibble(time = times, !!shock_name := values)
    } else if (time_class == "Date") {
      if (is.na(shock_start)) shock_start <- min(initial_matrix$time)
      if (is.na(shock_end)) shock_end <- max(initial_matrix$time)

      times <- seq(as.Date(shock_start), as.Date(shock_end), by = "quarter")

      shock_type <- names(shock[[s]]$values)

      if (shock_type == "value") {
        values <- shock[[s]]$values$value
      } else if (shock_type == "rate") {
        values <- initial_matrix[initial_matrix$time %in% times, ][, s][[1]] *
          (1 + shock[[s]]$values$rate)
      } else if (shock_type == "absolute") {
        values <- initial_matrix[initial_matrix$time %in% times, ][, s][[1]] +
          shock[[s]]$values$absolute
      }

      if (length(values) > length(times)) {
        values <- values[1:length(times)]
      } else if (length(values) < length(times)) {
        values <- c(values, rep(tail(values, 1), length(times) - length(values)))
      }

      shock_tbl_s <- tibble::tibble(time = times, !!shock_name := values)
    }

    initial_matrix <- dplyr::full_join(shock_tbl_s, initial_matrix, by = "time") %>%
      dplyr::mutate(
        !!s := as.numeric(ifelse(is.na(get(!!shock_name)), get(!!s), get(!!shock_name)))
      ) %>%
      dplyr::arrange(time) %>%
      dplyr::select(-dplyr::all_of(shock_name))
  }

  initial_matrix <- dplyr::select(initial_matrix, -time)
  initial_matrix <- as.matrix(initial_matrix)
  rownames(initial_matrix) <- NULL
  initial_matrix <- initial_matrix[1:periods, ]
  model[[scenario]] <- list(initial_matrix = initial_matrix)

  return(model)
}
