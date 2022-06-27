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

  message("Added shock ", equation)

  return(shock)
}
