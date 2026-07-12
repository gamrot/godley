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


#' Newton and Broyden solver wrapper
#'
#' @author João Macalós
#'
#' @param m the initialized matrix obtained with \code{prepare()} or \code{prepare_scenario_matrix()}
#' @param calls prepared equations with \code{prepare()}
#' @param periods total number of rows (periods) in the model
#' @param max_iter maximum number of iterations allowed per block per period
#' @param tol tolerance accepted to determine convergence
#' @param deps solver-ready structures obtained with \code{.prep_nonlinear_blocks()}
#' @param method name of the algorithm used to solve the cyclical blocks, 'Newton' or 'Broyden'
#'
#' @details This function simulates the model by recursion by using nested
#' for loops, the same way as \code{run_gauss_seidel()}. CND statements and
#' acyclical blocks are deterministic and evaluated directly. The cyclical
#' blocks of equations are solved with \code{.newton_solver()} (based on
#' \code{multiroot()} from \code{rootSolve}) or with \code{.broyden_solver()}.
#' Both methods share the same \code{block_foo()}, but \code{multiroot()}
#' expects \code{f(x)} while \code{jacobian.full()} expects
#' \code{f(time, x, parms)}, hence the small wrapper on the Newton path.
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
                                  method = c("Newton", "Broyden"),
                                  ...) {
  
  method <- match.arg(method)
  
  exs_l <- deps$exs_l
  
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
  
  stop_if_not_finite <- function(vals, .ids, .i) {
    if (any(is.na(vals) | !is.finite(vals))) {
      stop(method, " algorithm failed
During computation NaN or Inf was obtained in period ", .i, " in equation(s) for: ",
           paste0(calls$lhs[.ids], collapse = ", "), "
Please check if equations are correctly specified or change initial values")
    }
  }
  
  for (.i in 2:periods) {
    for (.b in deps$block_ids) {
      block <- deps$blocks[[.b]]
      .ids <- deps$equation_ids[[.b]]
      
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
          warning(method, " algorithm did not converge in period ", .i,
                  " in cyclical block with variables: ",
                  paste0(calls$lhs[.ids], collapse = ", "), "
Reached max_iter = ", max_iter, " without meeting the tolerance
Please increase max_iter, change initial values or try a different method")
        }
        
        for (.v in seq_along(x$x)) {
          m[.i, .ids[[.v]]] <- x$x[.v]
        }
      }
    }
  }
  
  return(m)
}