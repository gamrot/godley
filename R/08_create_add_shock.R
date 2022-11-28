#' Create shock used in \code{add_scenario()}.
#'
#' @export
#'
#' @return shock object
#'

create_shock <- function() {
  shock <- tibble::tibble(
    "equation" = character(), "start" = numeric(),
    "end" = numeric(), "desc" = character()
  )

  message("Shock object created")

  return(shock)
}

#' Add shock to shock object
#'
#' @export
#'
#' @param shock tibble from \code{create_shock()}
#' @param equation string equation in format: 'x = 2'
#' @param start numeric period number for the shock to take place, defaults to NA
#' @param end numeric period number for the shock to take place, defaults to NA
#' @param desc string variable description
#'
#' @return updated shock object containing added shock

add_shock <- function(shock,
                      equation,
                      start = NA,
                      end = NA,
                      desc = "") {

  # argument check
  # type
  checkmate::assert_string(equation)
  checkmate::assert_int(start, na.ok = T, lower = 0)
  checkmate::assert_int(end, na.ok = T, lower = 0)
  checkmate::assert_string(desc)
  # conditions
  if (!is.na(start) & !is.na(end) & start > end) {
    stop("Start cannot be after end")
  }

  new_row <- tibble::tibble(
    "equation" = equation, "start" = start,
    "end" = end, "desc" = desc
  )
  shock <- tibble::add_row(shock, new_row)

  return(shock)
}

#' Add vector as a shock to shock object
#'
#' @export
#'
#' @param shock tibble from \code{create_shock()}
#' @param variable variable
#' @param values values
#' @param start numeric period number for the shock to take place, defaults to NA
#' @param end numeric period number for the shock to take place, defaults to NA
#' @param desc string variable description
#'
#' @return updated shock object containing added shock

add_shock_vector <- function(shock,
                             variable,
                             values,
                             start = 1,
                             desc = "") {
  # argument check
  # type
  checkmate::assert_string(variable)
  checkmate::assert_vector(values)
  checkmate::assert_int(start, lower = 0)
  checkmate::assert_string(desc)

  for (i in 1:length(values)) {
    eq <- paste0(variable, "=", as.character(values[i]))
    shock <- shock %>% godley:::add_shock(
      equation = eq,
      start = start + i - 1,
      end = start + i - 1,
      desc = desc
    )
  }

  return(shock)
}
