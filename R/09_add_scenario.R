#' Add scenario to the model
#'
#' @export
#'
#' @param model SFC model object
#' @param name string name of scenario, defaults to 'expansion'
#' @param origin string name of origin scenario, from which the new scenario will be created, defaults to 'baseline'
#' @param origin_start numeric period number from origin scenario from which the new scenario will begin
#' @param origin_end numeric period number from origin scenario on which the new scenario will end
#' @param shock shock object from \code{create_shock()} and \code{add_shock()}
#'
#' @return updated SFC model object containing added scenario

add_scenario <- function(model,
                         name = "expansion",
                         origin = "baseline",
                         origin_start = NA,
                         origin_end = NA,
                         shock) {
  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_string(name)
  checkmate::assert_string(origin)

  checkmate::assert(
    checkmate::check_int(origin_start, na.ok = T, lower = 0),
    checkmate::check_string(origin_start, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}"),
    checkmate::check_date(origin_start)
  )
  if (
    checkmate::test_string(origin_start, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}") |
      checkmate::test_date(origin_start)
  ) {
    checkmate::assert_date(as.Date(origin_start))
    origin_start <- as.Date(origin_start)
  }

  checkmate::assert(
    checkmate::check_int(origin_end, na.ok = T, lower = 0),
    checkmate::check_string(origin_end, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}"),
    checkmate::check_date(origin_end)
  )
  if (
    checkmate::test_string(origin_end, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}") |
      checkmate::test_date(origin_end)
  ) {
    checkmate::assert_date(as.Date(origin_end))
    origin_end <- as.Date(origin_end)
  }

  checkmate::assert_class(shock, "SFC_shock")

  if (!(is.null(model[[name]]$result))) {
    warning("Scenario named ", name, " already exists and will be overwritten")
  }

  if (is.null(model[[origin]]$result)) {
    stop("Origin scenario ", origin, " has not been simulated yet
Please simulate scenario before incorporating a shock")
  }

  no_names <- lubridate::setdiff(names(shock), model$variables$name)
  if (length(no_names) > 0) {
    stop(paste0("There is no variable named ", paste0(no_names, collapse = ", "), " in the model"))
  }

  if (all(!is.na(c(origin_start, origin_end)))) {
    if (class(origin_start) != class(origin_end)) {
      stop("Start and end are not of the same class, choose either numeric or Date")
    }
    if (origin_start > origin_end) {
      stop("Start cannot be after end")
    }
  }

  # get time types of all objects
  time_class_model <- class(model$baseline$result$time[1])
  if (all(is.na(c(origin_start, origin_end)))) {
    time_class_scenario <- time_class_model
  } else if (is.numeric(origin_start) | is.numeric(origin_end)) {
    time_class_scenario <- "numeric"
  } else {
    time_class_scenario <- "Date"
  }
  time_class_shock <- attr(shock, "time_class_shock")
  if (is.na(time_class_shock)) {
    time_class_shock <- time_class_model
  }

  # check if all are identical
  if (!all(
    identical(time_class_model, time_class_scenario),
    identical(time_class_model, time_class_shock)
  )) {
    time_class <- c(
      "model" = time_class_model,
      "scenario" = time_class_scenario,
      "shock" = time_class_shock
    )
    time_double <- names(which(time_class == "numeric"))
    time_date <- names(which(time_class == "Date"))
    stop(
      paste0(
        "Time types should be the same. These are int: ",
        paste0(time_double, collapse = ", "), ". These are dates: ",
        paste0(time_date, collapse = ", ")
      )
    )
  }
  time_class <- time_class_model

  # create initial matrix from origin
  if (time_class == "numeric") {
    if (is.na(origin_start)) origin_start <- 1
    if (is.na(origin_end)) origin_end <- nrow(model[[origin]]$result)

    initial_matrix <- model[[origin]]$result %>%
      dplyr::filter(time %in% c(origin_start:origin_end))
  } else if (time_class == "Date") {
    if (is.na(origin_start)) origin_start <- min(model[[origin]]$result$time)
    if (is.na(origin_end)) origin_end <- max(model[[origin]]$result$time)

    initial_matrix <- model[[origin]]$result %>%
      dplyr::filter(time %in% seq(as.Date(origin_start), as.Date(origin_end), by = "quarter"))
  }

  attr(shock, "time_class_shock") <- time_class

  model[[name]] <- list(initial_matrix = initial_matrix, shock = shock, origin = origin)

  return(model)
}
