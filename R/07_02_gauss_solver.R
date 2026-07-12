#' Restore equation notation for warning messages
#'
#' This function converts an internally processed equation expression (e.g., `m[.i, 'Var']`)
#' into a more readable format (e.g., `Var`). The function handles lagged variables,
#' transforming expressions such as `m[.i - 1, 'Var']` into `Var[-1]`.
#' It is intended for use in warning messages.
#'
#' @param eq_line A character string representing an equation with internal indexing.
#'
#' @return A character string where the internal indexing notation has been restored to
#' a more user-friendly form.
#' @keywords internal
restore_equation <- function(eq_line) {
  # Replace m[.i - k, 'Var'] or m[.i - k, "Var"] with Var[-k]
  eq_line_mod <- gsub("m\\[\\.i\\s*-\\s*(\\d+)\\s*,\\s*['\"](.*?)['\"]\\]", "\\2[-\\1]", eq_line)

  # Replace m[.i, 'Var'] or m[.i, "Var"] with Var
  eq_line_mod <- gsub("m\\[\\.i\\s*,\\s*['\"](.*?)['\"]\\]", "\\1", eq_line_mod)

  return(eq_line_mod)
}


#' Gauss-Seidel solver algorithm (single cyclical block, single period)
#'
#' Runs the Gauss-Seidel fixed-point iteration for one cyclical block of
#' equations in one simulation period. Equations are evaluated one by one
#' and results are written into the matrix immediately, so that subsequent
#' equations within the same sweep see the updated values -- this sequential
#' updating is the defining feature of the method and must not be
#' parallelised or vectorised.
#'
#' The convergence bookkeeping (`checks`, `holdouts`) is deliberately
#' engine-level state that persists across blocks and periods (matching the
#' historical implementation); this function receives the current state and
#' returns the updated one.
#'
#' @author João Macalós
#'
#' @param m The scenario matrix being simulated.
#' @param .i Current period (row of \code{m}).
#' @param .ids Equation ids (matrix columns) of the cyclical block.
#' @param exs List of parsed right-hand-side expressions, indexed by
#'   equation id.
#' @param checks Named numeric vector of last relative changes per variable.
#' @param holdouts Named numeric vector of reference values per variable.
#' @param max_iter Maximum number of sweeps allowed for this block.
#' @param tol Tolerance accepted to determine convergence.
#' @param verbose If TRUE, print the progress of the algorithm.
#' @param calls Prepared equations (used for readable error messages).
#'
#' @return A list with elements \code{m}, \code{checks}, \code{holdouts}
#'   (updated state) and \code{converged} (logical).
#'
#' @keywords internal
#'
.gauss_solver <- function(m, .i, .ids, exs, checks, holdouts,
                          max_iter, tol, verbose, calls) {

  converged <- FALSE

  for (.j in 1:max_iter) {
    for (.v in .ids) {
      if (verbose == TRUE) {
        # At the start of each iteration, print an iteration header
        if (.v == .ids[1]) {
          if (.j == 1) message("Period: ", .i)
          message(" Iteration: ", .j)
        }

        # Print each variable on its own line, indented for clarity
        message("   ", calls$lhs[.v], ": value = ", m[.i, .v])
      }

      # NOTE: the expression is deliberately evaluated twice (test + assign),
      # exactly as in the historical implementation. Do NOT collapse this
      # into a single evaluation: equations may contain stochastic terms
      # (e.g. rnorm() in the PCEX template), so the number of evaluations is
      # part of the reproducible arithmetic path.
      if (!checkmate::test_number(suppressMessages(eval(exs[[.v]])), na.ok = TRUE)) next

      m[.i, .v] <- suppressMessages(eval(exs[[.v]]))

      if (is.na(m[.i, .v]) | !is.finite(m[.i, .v])) {
        stop(
          "Gauss algorithm failed.\n",
          "During computation NaN or Inf was obtained in period ", .i,
          " in cyclical block with variables: ",
          paste0(calls$lhs[.ids], collapse = ", "), "\n",
          "in equation for ", calls$lhs[.v], ":\n",
          restore_equation(paste(deparse(exs[[.v]]), collapse = " ")), "\n",
          "Please check if equations are correctly specified or change initial values"
        )
      }

      checks[[.v]] <- suppressMessages(abs(m[.i, .v] - holdouts[[.v]]) / (holdouts[[.v]] + 1e-05))
    }

    if (any(!is.finite(checks[.ids]) | is.na(checks[.ids]))) {
      stop(
        "Gauss algorithm failed to converge.\n",
        "During computation NaN or Inf was obtained in period ", .i,
        " in convergence checks for cyclical block with variables: ",
        paste0(calls$lhs[.ids], collapse = ", "), "\n",
        "Please check the initial values to exclude any division by zero ",
        "or other invalid operations, or try a different method"
      )
    }

    if (all(checks[.ids] < tol)) {
      converged <- TRUE
      break
    } else {
      for (.v in .ids) {
        holdouts[[.v]] <- m[.i, .v]
      }
    }
  }

  return(list(m = m, checks = checks, holdouts = holdouts, converged = converged))
}
