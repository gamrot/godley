test_that("Newton and Broyden solve the same toy system", {
  # known root: x = (1, 2)
  fn3 <- function(.time, .x, parms) {
    c(.x[1] + .x[2] - 3, .x[1] * .x[2] - 2)
  }
  fn1 <- function(.x) fn3(0, .x, NULL)

  x0 <- c(0.5, 2.5)

  nw <- .newton_solver(x0, fn1, max_iter = 100, tol = 1e-10)
  br <- .broyden_solver(x0, fn3, max_iter = 100, tol = 1e-10)

  expect_true(nw$converged)
  expect_true(br$converged)
  expect_equal(sort(nw$x), c(1, 2), tolerance = 1e-6)
  expect_equal(sort(as.numeric(br$x)), c(1, 2), tolerance = 1e-6)
})

test_that("Newton and Broyden agree on SIM", {
  model <- create_model(name = "SIM", template = "SIM")

  m_newton <- simulate_scenario(model,
    scenario = "baseline",
    periods = 60, method = "Newton"
  )
  m_broyden <- simulate_scenario(model,
    scenario = "baseline",
    periods = 60, method = "Broyden"
  )

  expect_equal(
    m_newton$baseline$result,
    m_broyden$baseline$result,
    tolerance = 1e-4
  )
})

test_that("Newton and Broyden stop on NaN or Inf", {
  # division by zero gives Inf in the first period
  broken <- create_model(name = "broken")
  broken <- add_variable(broken, "y", init = 1)
  broken <- add_variable(broken, "z", init = 0)
  broken <- add_equation(broken, "y = y[-1] / z")

  for (mth in c("Newton", "Broyden")) {
    expect_error(
      suppressWarnings(
        simulate_scenario(broken,
          scenario = "baseline",
          periods = 5, method = mth
        )
      ),
      regexp = "NaN or Inf"
    )
  }
})

test_that("Broyden warns when max_iter is exhausted", {
  model <- create_model(name = "SIM", template = "SIM")

  # with max_iter = 1 the solver cannot converge and should warn
  warns <- character(0)
  res <- tryCatch(
    withCallingHandlers(
      simulate_scenario(model,
        scenario = "baseline",
        periods = 10, method = "Broyden", max_iter = 1, tol = 1e-12
      ),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  expect_true(any(grepl("max_iter", warns)))

  # an error from the hidden equations check is fine here, anything else is not
  if (inherits(res, "error")) {
    expect_match(conditionMessage(res), "hidden", ignore.case = TRUE)
  }
})

test_that("Newton runs silently", {
  # no stray print() output allowed
  model <- create_model(name = "SIM", template = "SIM")

  expect_silent(
    suppressMessages(suppressWarnings(
      simulate_scenario(model,
        scenario = "baseline",
        periods = 10, method = "Newton"
      )
    ))
  )
})
