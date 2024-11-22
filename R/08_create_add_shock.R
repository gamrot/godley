#' Create shock used in \code{add_scenario()}.
#'
#' @export
#'
#' @return shock object
#'

create_shock <- function() {
  shock <- structure(list(), class = "SFC_shock")

  message("Shock object created")

  return(shock)
}

#' Add shock to shock object
#'
#' @export
#'
#' @param shock tibble from \code{create_shock()}
#' @param variable string variable name
#' @param value numeric, an explicit value or values for the variable, will be extended with last value
#' @param rate numeric, multiplier to influence the original value of the variable
#' @param absolute numeric, absolute value to influence the original value of the variable
#' @param start numeric or date period number for the shock to take place, defaults to NA
#' @param end numeric or date period number for the shock to take place, defaults to NA
#' @param desc string variable description
#'
#' @return updated shock object containing added shock

add_shock <- function(shock,
                      variable,
                      value = NA,
                      rate = NA,
                      absolute = NA,
                      start = NA,
                      end = NA,
                      desc = "") {
  # argument check
  # type
  checkmate::assert_class(shock, "SFC_shock")
  checkmate::assert_character(variable,
    any.missing = F, all.missing = F,
    unique = T
  )
  checkmate::assert_numeric(value)
  checkmate::assert_number(rate, na.ok = T)
  checkmate::assert_number(absolute, na.ok = T)

  checkmate::assert(
    checkmate::check_int(start, na.ok = T, lower = 0),
    checkmate::check_string(start, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}"),
    checkmate::check_date(start)
  )
  if (
    checkmate::test_string(start, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}") |
      checkmate::test_date(start)
  ) {
    checkmate::assert_date(as.Date(start))
    start <- as.Date(start)
  }

  checkmate::assert(
    checkmate::check_int(end, na.ok = T, lower = 0),
    checkmate::check_string(end, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}"),
    checkmate::check_date(end)
  )
  if (
    checkmate::test_string(end, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}") |
      checkmate::test_date(end)
  ) {
    checkmate::assert_date(as.Date(end))
    end <- as.Date(end)
  }

  checkmate::assert_string(desc)

  # conditions
  if (all(!is.na(c(start, end)))) {
    if (class(start) != class(end)) {
      stop("Start and end are not of the same class, choose either numeric or Date")
    }
    if (start > end) {
      stop("Start cannot be after end")
    }
  }

  if (all(is.na(c(start, end)))) {
    time_class_shock <- NA
  } else if (is.numeric(start) | is.numeric(end)) {
    time_class_shock <- "numeric"
  } else {
    time_class_shock <- "Date"
  }

  shock_type <- list(value = value, rate = rate, absolute = absolute)
  if (sum(!is.na(shock_type)) != 1) {
    stop("Exactly one of the following must be specified: value, rate, absolute")
  }
  values <- shock_type[which(!is.na(shock_type))]

  times <- list(start = start, end = end)

  for (v in variable) {
    shock[[v]] <- list(values = values, times = times)
  }

  shock <- structure(shock, time_class_shock = time_class_shock)

  return(shock)
}
