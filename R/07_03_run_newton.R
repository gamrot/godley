#' Newton solver algorithm
#'
#' @param .x0 Vector with initial guess for x.
#' @param .fn A function containing the system of equations.
#' @param max_iter Maximum number of iterations allowed
#' @param tol A numeric value indicating the accepted tolerance to declare convergence.
#'
#' @note Check https://www.math.usm.edu/lambers/mat419/lecture11.pdf for a quick reference
#' on the algorithm.
#'
#' @author ...
#'
#' @keywords internal
#'
.newton_solver <- function(.x0, .fn, max_iter, tol) {
  print("Running Newton Solver")
  x <- rootSolve::multiroot(.fn, .x0, max_iter, ctol = tol)
  return(list(x = x$root))
}


# ' Newton Raphson solver implemented with \code{rootSolve::multiroot()}
# '
# ' @author João Macalós
# '
# ' @param m the initialized matrix obtained with code{prepare()} or \code{prepare_scenario_matrix()}
# ' @param calls prepared equations with \code{prepare()}
# ' @param periods total number of rows (periods) in the model
# ' @param max_iter maximum number of iterations allowed per block per period
# ' @param tol tolerance accepted to determine convergence
# '
# ' @details This function implements the Newton-Raphson method to solve the cyclical
# ' blocks of equations. It relies on the \code{multiroot()} function from \code{rootSolve}.
# '
# ' @return simulated scenario matrix

run_newton <- function(m,
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
          stop("Newton algorithm failed
During computation NaN or Inf was obtained in ", idvar_, " equation
Please check if equations are correctly specified or change initial values")
        }
      } else {
        # If acyclical block --> deterministic
        if (vctrs::vec_size(block) == 1) {
          m[.i, idvar_] <- eval(exs_l[[.b]][[1]])

          if (is.na(m[.i, idvar_]) | !is.finite(m[.i, idvar_])) {
            stop("Newton algorithm failed
During computation NaN or Inf was obtained in ", idvar_, " equation
Please check if equations are correctly specified or change initial values")
          }
        } else {
          xstart <- m[.i-1, idvar_]
          exs <- exs_nl[[.b]]

          x <- .newton_solver(xstart, block_foo, max_iter, tol)

          for (.v in seq_along(x$x)) {
            m[.i, idvar_[[.v]]] <- x$x[.v]
          }
        }
      }
    }
  }

  return(m)
}
