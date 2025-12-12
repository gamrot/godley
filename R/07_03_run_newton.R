#' Prep equations for Newton solvers
#'
#' @author João Macalós
#'
#' @param .block blocks of equations
#'
#' @return blocks

prep_broyden <- function(.block) {
  for (.i in seq_len(vctrs::vec_size(.block))) {
    .block$rhs2 <- gsub(.block$lhs2[[.i]], paste0(".x\\[", .i, "\\]"), .block$rhs2)
  }
  return(.block)
}

#' Newton Raphson and Broyden solver implemented with \code{nleqslv::nleqslv()}
#'
#' @author João Macalós
#' @editor Iwo Augustyński (2025) - switched from rootSolve::multiroot to nleqslv::nleqslv
#' @importFrom nleqslv nleqslv
#'
#' @param m the initialized matrix obtained with code{prepare()} or \code{prepare_scenario_matrix()}
#' @param calls prepared equations with \code{prepare()}
#' @param method either "Newton" or "Broyden"
#' @param periods total number of rows (periods) in the model
#' @param max_iter maximum number of iterations allowed per block per period
#' @param tol tolerance accepted to determine convergence
#' @param global character, global strategy for nleqslv, see \code{nleqslv::nleqslv()} documentation, defaults to "dbldog"
#' @param ... additional arguments passed to \code{nleqslv()} control parameters, see \code{nleqslv::nleqslv()} documentation
#'
#' @details This function implements the Newton-Raphson and Broyden methods to solve the cyclical
#' blocks of equations. It relies on the \code{nleqslv()} function from \code{nleqslv}.
#'
#' @return simulated scenario matrix

run_newton_broyden <- function(m,
                       calls,
                       method,
                       periods,
                       max_iter,
                       tol,
                       global = "dbldog",
                       ...) {
  blocks <- unique(sort(calls$block))

  equations_id <- purrr::map(blocks, ~ calls[, "id"][calls[, "block"] == .x])

  cnd_statements <- calls %>%
    dplyr::filter(
      stringr::str_detect(.data$rhs, "if"),
      stringr::str_detect(.data$rhs, "else")
    ) %>%
    dplyr::pull(block)

  eqs2 <- calls %>%
    dplyr::mutate(lhs2 = gsub(.pvar(.data$lhs), "m\\[.i, '\\1'\\]", .data$lhs, perl = T)) %>%
    dplyr::mutate(rhs2 = paste0(.data$rhs, " - ", .data$lhs2)) %>%
    dplyr::mutate(lhs2 = stringr::str_replace_all(.data$lhs2, c("\\[" = "\\\\[", "\\]" = "\\\\]")))

  blk <- purrr::map(blocks, ~ eqs2[eqs2$block == .x, ])

  blk <- purrr::map(blk, prep_broyden)

  block_names <- purrr::map(blocks, ~ paste0("block", .x))

  ## Parsed non-linear expressions (for nleqslv)
  exs_nl <- purrr::map(blk, function(.X) purrr::map(.X$rhs2, ~ rlang::parse_expr(.x)))

  ## Parsed linear expressions (for Gauss Seidel)
  exs_l <- purrr::map(blk, function(.X) purrr::map(.X$rhs, ~ rlang::parse_expr(.x)))

  block_foo <- function(.x) {
    .y <- numeric(length(exs))
    for (.id in seq_along(exs)) {
      .y[.id] <- eval(exs[[.id]])
    }
    .y
  }


  for (.i in 2:periods) {
    for (.b in blocks) {
      block <- blk[[.b]]
      idvar_ <- equations_id[[.b]]

      ## CND statement must be dealt separately
      if (.b %in% cnd_statements) {
        m[.i, idvar_] <- eval(exs_l[[.b]][[1]])

        if (is.na(m[.i, idvar_]) | !is.finite(m[.i, idvar_])) {
          stop("Algorithm failed
During computation NaN or Inf was obtained in ", idvar_, " equation
Please check if equations are correctly specified or change initial values")
        }
      } else {
        # If acyclical block --> deterministic
        if (vctrs::vec_size(block) == 1) {
          m[.i, idvar_] <- eval(exs_l[[.b]][[1]])

          if (is.na(m[.i, idvar_]) | !is.finite(m[.i, idvar_])) {
            stop("Algorithm failed
During computation NaN or Inf was obtained in ", idvar_, " equation
Please check if equations are correctly specified or change initial values")
          }
        } else {
          xstart <- m[.i - 1, idvar_]
          exs <- exs_nl[[.b]]

          #x <- rootSolve::multiroot(block_foo, xstart, max_iter, ctol = tol)
          tryCatch(
            {x <- nleqslv::nleqslv(x = xstart, fn = block_foo, jac = NULL, method = method, 
                                   global = global, control = list(maxit = max_iter, xtol = tol, allowSingular = TRUE))
            },
            error = function(e) {
              pattern <- "(?<=index=)\\d+"
              extracted_number <- as.numeric(stringr::str_extract(conditionMessage(e), pattern))
              eq <- as.character(exs[[extracted_number]])[2]
              cat(glue::glue("The solver did not converge.\n During computation NaN or Inf was obtained in equation:\n {eq}\n Please check if equations are correctly specified or change initial values"))
            }
          )

          # for (.v in seq_along(x$root)) {
          #   m[.i, idvar_[[.v]]] <- x$root[.v]
          #   # m[.i, block_names[[.b]]] <- x$iter
          # }
          for (.v in seq_along(x$x)) {
            m[.i, idvar_[[.v]]] <- x$x[.v]

          }
        }
      }
    }
  }

  return(m)
}
