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
#' @keywords internal
#'
.newton_solver <- function(.x0, .fn, max_iter, tol) {
  x <- rootSolve::multiroot(.fn, .x0, maxiter = max_iter, ctol = tol)
  
  # multiroot() does not report convergence -- hitting max_iter or a
  # non-finite root counts as a failure
  converged <- x$iter < max_iter && all(is.finite(x$root))
  
  return(list(x = x$root, converged = converged))
}


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
  
  # NA means NaN showed up in the iterates -- exit the loop like the
  # original code did; non-finite results are caught in .run_nonlinear_solver()
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


#' Gauss-Seidel, Newton and Broyden solver wrapper
#'
#' @author João Macalós
#'
#' @param m the initialized matrix obtained with \code{prepare()} or \code{prepare_scenario_matrix()}
#' @param calls prepared equations with \code{prepare()}
#' @param periods total number of rows (periods) in the model
#' @param max_iter maximum number of iterations allowed per block per period
#' @param tol tolerance accepted to determine convergence
#' @param deps solver-ready structures obtained with \code{.prep_nonlinear_blocks()}
#' @param method name of the algorithm used to solve the cyclical blocks, 'Gauss', 'Newton' or 'Broyden'
#' @param verbose if TRUE, print the progress of the algorithm (Gauss only)
#'
#' @details This function simulates the model by recursion by using nested
#' for loops. CND statements and acyclical blocks are deterministic and
#' evaluated directly. The cyclical blocks of equations are solved with
#' \code{.gauss_solver()}, \code{.newton_solver()} (based on \code{multiroot()}
#' from \code{rootSolve}) or \code{.broyden_solver()}, depending on the method
#' chosen by the user. Newton and Broyden share the same \code{block_foo()},
#' but \code{multiroot()} expects \code{f(x)} while \code{jacobian.full()}
#' expects \code{f(time, x, parms)}, hence the small wrapper on the Newton
#' path. The Gauss-Seidel convergence trackers (\code{checks} and
#' \code{holdouts}) live here and are carried across blocks and periods,
#' like in the original \code{run_gauss_seidel()}.
#'
#' @return simulated scenario matrix
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
    # convergence trackers are shared across blocks and periods,
    # like in the original run_gauss_seidel()
    checks <- rep(0, length(calls$lhs))
    names(checks) <- calls$lhs
    
    holdouts <- c(m[1, 1:vctrs::vec_size(calls$lhs)])
    names(holdouts) <- calls$lhs
    
    # flatten the per-block expressions so they can be indexed by equation id
    exs_flat <- vector("list", length(calls$lhs))
    for (.b in seq_along(deps$block_ids)) {
      .ids <- deps$equation_ids[[.b]]
      for (.k in seq_along(.ids)) {
        exs_flat[[.ids[[.k]]]] <- exs_l[[.b]][[.k]]
      }
    }
  } else {
    # exs is set per block in the loop below
    exs <- NULL
    
    # block_foo() takes (.time, .x, parms) because jacobian.full() calls it
    # this way; multiroot() calls f(x), hence the wrapper on the Newton path
    block_foo <- function(.time, .x, parms) {
      .y <- numeric(length(exs))
      for (.id in seq_along(exs)) {
        .y[.id] <- eval(exs[[.id]])
      }
      .y
    }
    
    solve_block <- switch(method,
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
      stop(method, " algorithm failed
During computation NaN or Inf was obtained in period ", .i, " in equation(s) for: ",
           paste0(calls$lhs[.ids], collapse = ", "), "
Please check if equations are correctly specified or change initial values")
    }
  }
  
  warn_not_converged <- function(.ids, .i) {
    warning(method, " algorithm did not converge in period ", .i,
            " in cyclical block with variables: ",
            paste0(calls$lhs[.ids], collapse = ", "), "
Reached max_iter = ", max_iter, " without meeting the tolerance
Please increase max_iter, change initial values or try a different method")
  }
  
  for (.i in 2:periods) {
    for (.b in deps$block_ids) {
      block <- deps$blocks[[.b]]
      .ids <- deps$equation_ids[[.b]]
      
      if (method == "Gauss") {
        
        ## acyclical block --> deterministic
        ## (if/else statements need no special handling on this path)
        if (length(.ids) == 1) {
          # evaluated twice on purpose (test + assign) -- see .gauss_solver()
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
        
        ## CND statements and acyclical blocks --> deterministic
        if (.b %in% deps$cnd_statements || vctrs::vec_size(block) == 1) {
          m[.i, .ids] <- eval(exs_l[[.b]][[1]])
          
          stop_if_not_finite(m[.i, .ids], .ids, .i)
        } else {
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