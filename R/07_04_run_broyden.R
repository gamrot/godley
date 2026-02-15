#' Broyden solver algorithm
#'
#' @param .x0 Vector with initial guess for x.
#' @param .fn A function containing the system of equations.
#' @param max_iter Maximum number of iterations allowed
#' @param tol A numeric value indicating the accepted tolerance to declare convergence.
#'
#' @note Check https://www.math.usm.edu/lambers/mat419/lecture11.pdf for a quick reference
#' on the algorithm.
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.broyden_solver <- function(.x0, .fn, max_iter, tol) {

  # First round
  D0 <- rootSolve::jacobian.full(.x0, .fn)
  g0 <- .fn(.x = .x0)
  D0inv <- solve_armadillo(D0)
  d0 <- -D0inv %*% g0

  nx <- .x0 + d0

  ite <- 1

  if (isFALSE(all(purrr::map2_lgl(.x0, nx, ~{abs(.x - .y)/(.y + 1e-15) < tol})))) {

    x0 <- nx

    # Iterator
    for (.ite in 1:max_iter) {
      ite <- .ite + 1

      ng <- .fn(.x = x0)
      u0 <- D0inv %*% ng
      c0 <- t(d0) %*% (d0 + u0)
      term1 <- u0 %*% t(d0)
      term1 <- term1 / c(c0)
      D1inv <- D0inv - term1 %*% D0inv

      d1 = -D1inv %*% ng
      nx <- x0 + d1

      if (isFALSE(all(purrr::map2_lgl(x0, nx, ~{abs(.x - .y)/(.y + 1e-15) < tol})))) {
        x0 <- nx
      } else {
        break
      }
    }

  }

  return(list(x = nx, ite = ite))

}


#' Broyden solver wrapper
#'
#' @param m The initialized matrix obtained with \code{.make_matrix()}.
#' @param calls Prepared equations with \code{.prep_equations()}.
#' @param periods Total number of rows (periods) in the model.
#' @param max_iter Maximum number of iterations allowed per block per period.
#' @param tol Tolerance accepted to determine convergence.
#'
#'
#' @details This function implements the Broyden method to solve the cyclical
#' blocks of equations.
#'
#' @author João Macalós
#'
#' @keywords internal
#'
run_broyden <- function(m,
                        calls,
                        periods,
                        max_iter,
                        tol,
                        dependencies,
                        ...) {
  
  blocks = dependencies$blocks
  equations_id = dependencies$equations_id 
  cnd_statements = dependencies$cnd_statements
  blk = dependencies$blk
  exs_nl = dependencies$exs_nl
  exs_l = dependencies$exs_l
  
  block_foo <- function(.time, .x, parms) {
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
      } else {
        if (vctrs::vec_size(block) == 1) {
          m[.i, idvar_] <- eval(exs_l[[.b]][[1]])
        } else {
          xstart <- m[.i-1, idvar_]
          exs <- exs_nl[[.b]]

          x <- .broyden_solver(xstart, block_foo, max_iter, tol)

          for (.v in seq_along(x$x)) {
            m[.i, idvar_[[.v]]] <- x$x[.v]
          }

        }
      }

    }

  }

  return(m)
}
