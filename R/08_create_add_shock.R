#' Create shock used in \code{add_scenario()}.
#'
#' @export
#'
#' @return shock object
#'

create_shock <- function() {
  shock <- tibble::tibble(
    "equation" = character(), "desc" = character(),
    "start" = numeric(), "end" = numeric()
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
#' @param desc string variable description
#' @param start numeric period number for the shock to take place, defaults to NA
#' @param end numeric period number for the shock to take place, defaults to NA
#'
#' @return updated shock object containing added shock

add_shock <- function(shock,
                      equation,
                      desc = "",
                      start = NA,
                      end = NA) {

  # argument check
  # type
  checkmate::assert_string(equation)
  checkmate::assert_string(desc)
  checkmate::assert_int(start, na.ok = T, lower = 0)
  checkmate::assert_int(end, na.ok = T, lower = 0)
  # conditions
  if (!is.na(start) & !is.na(end) & start > end) {
    stop("Start cannot be after end")
  }

  new_row <- tibble::tibble(
    "equation" = equation, "desc" = desc, "start" = start,
    "end" = end
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
#' @param desc string variable description
#' @param start numeric period number for the shock to take place, defaults to NA
#' @param end numeric period number for the shock to take place, defaults to NA
#'
#' @return updated shock object containing added shock

add_shock_vector <- function(shock,
                             variable,
                             values,
                             desc = "",
                             start = 1) {
  # argument check
  # type
  checkmate::assert_string(variable)
  checkmate::assert_vector(values)
  checkmate::assert_string(desc)
  checkmate::assert_int(start, lower = 0)

  for (i in 1:length(values)) {
    eq <- paste0(variable, "=", as.character(values[i]))
    shock <- shock %>% godley:::add_shock(
      equation = eq,
      desc = desc,
      start = start + i - 1,
      end = start + i - 1
    )
  }

  return(shock)
}
