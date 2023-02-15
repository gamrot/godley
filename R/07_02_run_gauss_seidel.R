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

# ' Gauss Seidel algorithm
# '
# ' @author João Macalós
# '
# ' @param m the initialized matrix obtained with \code{prepare()} or \code{prepare_scenario_matrix()}
# ' @param calls prepared equations with \code{prepare()}
# ' @param periods total number of rows (periods) in the model
# ' @param max_iter maximum number of iterations allowed per block per period
# ' @param tol tolerance accepted to determine convergence
# ' @param verbose if TRUE, print the progress of the algorithm
# '
# ' @details This algorithm simulates the model by recursion by using
# ' nested for loops. At each round of iteration, the values calculated
# ' are compared to the previous values. If the difference is below
# ' a tolerance value set by the user, the round of calculations have converged
# ' and the algorithm jump to the next block of equations.
# ' The algorithm modifies a matrix in place to optimize its performance.
# '
# ' @return simulated scenario matrix

run_gauss_seidel <- function(m,
                             calls,
                             periods,
                             max_iter,
                             tol,
                             verbose = FALSE) {
  # checks
  checkmate::assert_matrix(m)
  checkmate::assert_number(periods, lower = 1)
  checkmate::assert_number(max_iter, lower = 1)
  checkmate::assert_numeric(tol)
  checkmate::assert_logical(verbose)

  exprs <- purrr::map(calls$rhs, function(x) parse(text = x))

  checks <- rep(0, length(calls$lhs))
  names(checks) <- calls$lhs

  holdouts <- rep(3, length(calls$lhs))
  holdouts <- c(m[1, 1:vctrs::vec_size(calls$lhs)])
  names(holdouts) <- calls$lhs

  blocks <- unique(sort(calls$block))
  equations_id <- lapply(blocks, function(x) calls[, "id"][calls[, "block"] == x])
  block_names <- lapply(blocks, function(x) paste0("block", x))

  # safe check

  for (.i in 2:periods) {
    for (.block in seq_along(blocks)) {
      .id <- equations_id[[.block]]

      # If 1 variable in the block, it is deterministic and no iteration is required.
      if (length(.id) == 1) {
        if (!checkmate::test_number(eval(exprs[[.id]]), na.ok = T)) next

        m[.i, .id] <- eval(exprs[[.id]])
        # m[.i, block_names[[.block]]] <- 1

        if (is.na(m[.i, .id]) | !is.finite(m[.i, .id])) {
          warning(
            "\n Gauss-Seidel algorithm failed.",
            "\n During computation NaN or Inf was obtained in ",
            exprs[[.id]], " equation",
            "\n Please check if equations are correctly specified or change initial values"
          )
          return(m)
        }
      } else { # If cyclical block, use Gauss-Seidel algorithm
        for (.ite in 1:max_iter) {
          for (.v in .id) {
            # if (verbose == TRUE) {message("\r",calls$lhs[.v], "/period: ", .i," /iter:", .ite ,appendLF = T)}
            if (verbose == TRUE) {
              # At the start of each period, print a header once
              if (.ite == 1 && .v == .id[1]) {
                message("\nSimulating scenario expansion (1 of 1)")
                message("Period: ", .i)
              }

              # At the start of each iteration, print an iteration header
              if (.v == .id[1]) {
                message(" Iteration: ", .ite)
              }

              # Print each variable on its own line, indented for clarity
              message("   ", calls$lhs[.v], ": value = ", m[.i, .v])
            }


            if (!checkmate::test_number(suppressMessages(eval(exprs[[.v]])), na.ok = T)) next

            m[.i, .v] <- suppressMessages(eval(exprs[[.v]]))

            if (is.na(m[.i, .v]) | !is.finite(m[.i, .v])) {
              warning_message <- paste0(
                "\nGauss-Seidel algorithm failed in cyclical block with variables: ",
                paste0(calls$lhs[.id], collapse = ", "),
                "\nDuring computation NaN or Inf was obtained in equation for ",
                calls$lhs[.v], ":\n",
                restore_equation(as.character(exprs[[.v]])),
                "\nCheck if equations are correctly specified or change initial values."
              )

              if (!verbose) {
                warning_message <- paste0(
                  warning_message,
                  "\nFor more details, try running simulate_scenario(..., verbose = TRUE)."
                )
              }

              warning(message = warning_message)
              return(m)
            }
            checks[[.v]] <- suppressMessages(abs(m[.i, .v] - holdouts[[.v]]) / (holdouts[[.v]] + 1e-05))
          }

          # m[.i, block_names[[.block]]] <- .ite

          if (any(!is.finite(checks[.id]) | is.na(checks[.id]))) {
            warning(paste0(
              "Gauss-Seidel algorithm failed to converge.",
              "\nProblem occured in ",
              paste0(.id, collapse = ", "),
              " equations block.",
              "\n Please check the initial values to exclude any division by zero or other invalid operations.",
              "\n If the problem persists, try a different method."
            ))
            return(m)
          }

          if (all(checks[.id] < tol)) {
            break
          } else {
            for (.v in .id) {
              holdouts[[.v]] <- m[.i, .v]
            }
          }
        }
      }
    }
  }
  return(m)
}
