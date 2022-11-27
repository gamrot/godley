# ' Gauss Seidel algorithm
# '
# ' @author João Macalós
# '
# ' @param m the initialized matrix obtained with \code{prepare()} or \code{prepare_scenario_matrix()}
# ' @param calls prepared equations with \code{prepare()}
# ' @param periods total number of rows (periods) in the model
# ' @param max_iter maximum number of iterations allowed per block per period
# ' @param tol tolerance accepted to determine convergence
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
                             tol) {
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
        if (!(.i == 2 & stringr::str_detect(gsub(" ", "", as.character(exprs[[.id]])), "\\Qm[.i-2,\\E"))) {
          m[.i, .id] <- eval(exprs[[.id]])
          # m[.i, block_names[[.block]]] <- 1
          if (is.na(m[.i, .id]) | !is.finite(m[.i, .id])) {
            stop("Gauss-Seidel algorithm failed
During computation NaN or Inf was obtained in ", .id, " equation
Please check if equations are correctly specified or change initial values")
          }
        }
      }

      # If cyclical block, use Gauss-Seidel algorithm
      else {
        for (.ite in 1:max_iter) {
          for (.v in .id) {
            if (!(.i == 2 & stringr::str_detect(gsub(" ", "", as.character(exprs[[.v]])), "\\Qm[.i-2,\\E"))) {
              m[.i, .v] <- suppressMessages(eval(exprs[[.v]]))
              # if(is.na(m[.i, .v])){
              #  stop(message = paste('Gauss-Seidel algorithm failed.
              # During computation NaN was obtained in ', .v , ' equation ',
              # 'Check if equations are correctly specified or change initial values.'))
              # }

              checks[[.v]] <- suppressMessages(abs(m[.i, .v] - holdouts[[.v]]) / (holdouts[[.v]] + 1e-15))
            }
          }

          # m[.i, block_names[[.block]]] <- .ite

          if (any(!is.finite(checks[.id]) | is.na(checks[.id]))) {
            stop(paste(c("Gauss-Seidel algorithm failed to converge
Please check the initial values to exclude any division by zero or other invalid operations
Problem occured in", .id, "equations block
If the problem persists, try a different method"), collapse = ", "))
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
