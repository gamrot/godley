#' Newton solver algorithm
#'
#' Thin wrapper around \code{rootSolve::multiroot()}.
#'
#' @param .x0 Vector with initial guess for x.
#' @param .fn A function containing the system of equations, in the
#'   single-argument convention \code{function(.x)} required by
#'   \code{rootSolve::multiroot()}.
#' @param max_iter Maximum number of iterations allowed.
#' @param tol A numeric value indicating the accepted tolerance to declare convergence.
#'
#' @note Check https://www.math.usm.edu/lambers/mat419/lecture11.pdf for a quick
#' reference on the algorithm.
#'
#' @return A list with elements \code{x} (solution vector) and \code{converged}
#'   (logical convergence heuristic).
#'
#' @keywords internal
#'
.newton_solver <- function(.x0, .fn, max_iter, tol) {
  x <- rootSolve::multiroot(.fn, .x0, maxiter = max_iter, ctol = tol)

  # Heuristic: multiroot() does not return an explicit convergence flag.
  # We treat hitting the iteration cap or a non-finite root as non-convergence.
  converged <- x$iter < max_iter && all(is.finite(x$root))

  return(list(x = x$root, converged = converged))
}


#' Broyden solver algorithm
#'
#' @param .x0 Vector with initial guess for x.
#' @param .fn A function containing the system of equations, in the
#'   three-argument convention \code{function(.time, .x, parms)} required by
#'   \code{rootSolve::jacobian.full()}.
#' @param max_iter Maximum number of iterations allowed.
#' @param tol A numeric value indicating the accepted tolerance to declare convergence.
#'
#' @note Check https://www.math.usm.edu/lambers/mat419/lecture11.pdf for a quick
#' reference on the algorithm.
#'
#' @author João Macalós
#'
#' @return A list with elements \code{x} (solution vector), \code{ite}
#'   (number of iterations used) and \code{converged} (logical).
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

  # NOTE: !isFALSE(all(...)) deliberately treats NA (NaN in iterates) the same
  # way the original implementation did, i.e. as an exit condition. Non-finite
  # results are caught downstream in .run_nonlinear_solver().
  conv <- all(purrr::map2_lgl(.x0, nx, ~{abs(.x - .y)/(.y + 1e-15) < tol}))
  converged <- !isFALSE(conv)

  if (isFALSE(conv)) {

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

      conv <- all(purrr::map2_lgl(x0, nx, ~{abs(.x - .y)/(.y + 1e-15) < tol}))
      converged <- !isFALSE(conv)

      if (isFALSE(conv)) {
        x0 <- nx
      } else {
        break
      }
    }

  }

  return(list(x = nx, ite = ite, converged = converged))

}


#' Simulation engine for all solution methods (Gauss, Newton, Broyden)
#'
#' Shared block-wise simulation loop. Deterministic (single-equation) blocks
#' are evaluated directly. Cyclical blocks are handed to the requested
#' algorithm: the Gauss-Seidel fixed-point iteration
#' (\code{.gauss_solver()}), or a root-finding method on the residual system
#' f(x) = 0 (\code{.newton_solver()} / \code{.broyden_solver()}).
#'
#' The two root-finding algorithms consume the same objective function but
#' through different calling conventions imposed by \pkg{rootSolve}:
#' \code{multiroot()} calls \code{f(x, ...)}, while \code{jacobian.full()}
#' calls \code{func(time, y, parms)}. The canonical \code{block_foo()} below
#' uses the wider three-argument convention and a one-argument adapter is
#' constructed for the Newton path.
#'
#' The Gauss path evaluates the fixed-point expressions (\code{deps$exs_l})
#' sequentially with immediate in-place updates; conditional (if/else)
#' equations need no special handling there, so the \code{cnd_statements}
#' shortcut applies only to the root-finding methods.
#'
#' @param m The initialized matrix obtained with \code{prepare()} or
#'   \code{prepare_scenario_matrix()}.
#' @param calls Prepared equations obtained with \code{prepare()}.
#' @param periods Total number of rows (periods) in the model.
#' @param max_iter Maximum number of iterations allowed per block per period.
#' @param tol Tolerance accepted to determine convergence.
#' @param deps Solver-ready structures returned by \code{.prep_nonlinear_blocks()}.
#' @param method String, one of \code{"Gauss"}, \code{"Newton"} or \code{"Broyden"}.
#' @param verbose If TRUE, print the progress of the algorithm (Gauss only).
#'
#' @return Simulated scenario matrix.
#'
#' @keywords internal
#'
.run_nonlinear_solver <- function(m,
                                  calls,
                                  periods,
                                  max_iter,
                                  tol,
                                  deps,
                                  method = c("Gauss", "Newton", "Broyden"),
                                  verbose = FALSE,
                                  ...) {

  method <- match.arg(method)

  checkmate::assert_matrix(m)
  checkmate::assert_number(periods, lower = 1)
  checkmate::assert_number(max_iter, lower = 1)
  checkmate::assert_numeric(tol)
  checkmate::assert_logical(verbose)

  exs_l <- deps$exs_l

  if (method == "Gauss") {
    # Engine-level convergence state: persists across blocks AND periods,
    # matching the historical run_gauss_seidel() implementation exactly.
    checks <- rep(0, length(calls$lhs))
    names(checks) <- calls$lhs

    holdouts <- c(m[1, 1:vctrs::vec_size(calls$lhs)])
    names(holdouts) <- calls$lhs

    # Flatten per-block parsed expressions into an equation-id-indexed list,
    # so the Gauss path can address them the way the historical code did.
    exs_flat <- vector("list", length(calls$lhs))
    for (.b in seq_along(deps$block_ids)) {
      .ids <- deps$equation_ids[[.b]]
      for (.k in seq_along(.ids)) {
        exs_flat[[.ids[[.k]]]] <- exs_l[[.b]][[.k]]
      }
    }
  } else {
    # Bound per block inside the loop below; block_foo() finds it lexically.
    exs <- NULL

    # Canonical objective function in the deSolve-style convention required by
    # rootSolve::jacobian.full() (Broyden path).
    block_foo <- function(.time, .x, parms) {
      .y <- numeric(length(exs))
      for (.id in seq_along(exs)) {
        .y[.id] <- eval(exs[[.id]])
      }
      .y
    }

    solve_block <- switch(method,
      # Adapter: multiroot() calls f(x), so translate the one-argument
      # convention into the canonical three-argument one.
      Newton = function(.x0) {
        .newton_solver(.x0, function(.x) block_foo(0, .x, NULL), max_iter, tol)
      },
      Broyden = function(.x0) {
        .broyden_solver(.x0, block_foo, max_iter, tol)
      }
    )
  }

  stop_if_not_finite <- function(vals, .ids, .i) {
    if (any(is.na(vals) | !is.finite(vals))) {
      stop(
        method, " algorithm failed.\n",
        "During computation NaN or Inf was obtained in period ", .i,
        " in equation(s) for: ",
        paste0(calls$lhs[.ids], collapse = ", "), "\n",
        "Please check if equations are correctly specified or change initial values"
      )
    }
  }

  warn_not_converged <- function(.ids, .i) {
    warning(
      method, " algorithm reached max_iter = ", max_iter,
      " without meeting the tolerance in period ", .i,
      " in cyclical block with variables: ",
      paste0(calls$lhs[.ids], collapse = ", "), "\n",
      "Results for this block may be inaccurate. ",
      "Consider increasing max_iter, changing initial values, ",
      "or using a different method."
    )
  }

  for (.i in 2:periods) {
    for (.b in deps$block_ids) {
      block <- deps$blocks[[.b]]
      .ids <- deps$equation_ids[[.b]]

      if (method == "Gauss") {

        # Single-equation block: deterministic, evaluate directly.
        # (Conditional if/else equations need no shortcut on this path --
        # they are evaluated like any other expression.)
        if (length(.ids) == 1) {
          # Deliberate double evaluation (test + assign) -- see the note in
          # .gauss_solver(); the evaluation count is part of the reproducible
          # arithmetic path for stochastic equations.
          if (!checkmate::test_number(eval(exs_flat[[.ids]]), na.ok = TRUE)) next

          m[.i, .ids] <- eval(exs_flat[[.ids]])

          stop_if_not_finite(m[.i, .ids], .ids, .i)
        } else {
          res <- .gauss_solver(
            m, .i, .ids, exs_flat, checks, holdouts,
            max_iter, tol, verbose, calls
          )
          m <- res$m
          checks <- res$checks
          holdouts <- res$holdouts

          if (!isTRUE(res$converged)) {
            warn_not_converged(.ids, .i)
          }
        }

      } else {

        # Conditional (if/else) statements and acyclical (single-equation)
        # blocks are deterministic: evaluate directly, no root-finding needed.
        if (.b %in% deps$cnd_statements || vctrs::vec_size(block) == 1) {
          m[.i, .ids] <- eval(exs_l[[.b]][[1]])

          stop_if_not_finite(m[.i, .ids], .ids, .i)
        } else {
          # Cyclical block: solve the residual system f(x) = 0.
          xstart <- m[.i - 1, .ids]
          exs <- deps$exs_nl[[.b]]

          x <- solve_block(xstart)

          stop_if_not_finite(x$x, .ids, .i)

          if (!isTRUE(x$converged)) {
            warn_not_converged(.ids, .i)
          }

          for (.v in seq_along(x$x)) {
            m[.i, .ids[[.v]]] <- x$x[.v]
          }
        }

      }
    }
  }

  return(m)
}
