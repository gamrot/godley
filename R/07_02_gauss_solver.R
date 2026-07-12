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
  
  # Optional: convert exp(1)^(...) to exp(...)
  # eq_line_mod <- gsub("exp\\(1\\)\\^\\((.*?)\\)", "exp(\\1)", eq_line_mod)
  
  return(eq_line_mod)
}


#' Gauss Seidel algorithm
#'
#' @author João Macalós
#'
#' @param m the initialized matrix obtained with \code{prepare()} or \code{prepare_scenario_matrix()}
#' @param .i current period (row of the matrix)
#' @param .ids equation ids of the cyclical block
#' @param exs parsed equations, indexed by equation id
#' @param checks last relative change per variable
#' @param holdouts reference values per variable
#' @param max_iter maximum number of iterations allowed per block per period
#' @param tol tolerance accepted to determine convergence
#' @param verbose if TRUE, print the progress of the algorithm
#' @param calls prepared equations with \code{prepare()}
#'
#' @details This algorithm solves one cyclical block of equations by recursion.
#' At each round of iteration, the values calculated
#' are compared to the previous values. If the difference is below
#' a tolerance value set by the user, the round of calculations have converged
#' and the algorithm jump to the next block of equations.
#' The algorithm modifies a matrix in place to optimize its performance.
#' The convergence trackers (\code{checks}, \code{holdouts}) are shared across
#' blocks and periods, so they are passed in and returned together with the matrix.
#'
#' @return list with the updated matrix, checks, holdouts and a convergence flag
#'
#' @keywords internal
#'
.gauss_solver <- function(m, .i, .ids, exs, checks, holdouts,
                          max_iter, tol, verbose, calls) {
  
  converged <- FALSE
  
  for (.j in 1:max_iter) {
    for (.v in .ids) {
      if (verbose == TRUE) {
        # print an iteration header at the start of each round
        if (.v == .ids[1]) {
          if (.j == 1) message("Period: ", .i)
          message(" Iteration: ", .j)
        }
        
        message("   ", calls$lhs[.v], ": value = ", m[.i, .v])
      }
      
      # evaluated twice on purpose (test + assign), like in the original code --
      # with stochastic equations (e.g. rnorm() in PCEX) the number of draws
      # matters, so do not collapse this into one eval
      if (!checkmate::test_number(suppressMessages(eval(exs[[.v]])), na.ok = TRUE)) next
      
      m[.i, .v] <- suppressMessages(eval(exs[[.v]]))
      
      if (is.na(m[.i, .v]) | !is.finite(m[.i, .v])) {
        stop("Gauss algorithm failed
During computation NaN or Inf was obtained in period ", .i, " in equation for ",
             calls$lhs[.v], ":
", restore_equation(paste(deparse(exs[[.v]]), collapse = " ")), "
Please check if equations are correctly specified or change initial values")
      }
      
      checks[[.v]] <- suppressMessages(abs(m[.i, .v] - holdouts[[.v]]) / (holdouts[[.v]] + 1e-05))
    }
    
    if (any(!is.finite(checks[.ids]) | is.na(checks[.ids]))) {
      stop("Gauss algorithm failed
During computation NaN or Inf was obtained in period ", .i,
           " in convergence checks for cyclical block with variables: ",
           paste0(calls$lhs[.ids], collapse = ", "), "
Please check the initial values to exclude any division by zero or try a different method")
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