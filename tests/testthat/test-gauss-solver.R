test_that("Gauss and Newton agree on SIM", {
  model <- create_model(name = "SIM", template = "SIM")

  m_gauss <- simulate_scenario(model,
    scenario = "baseline",
    periods = 60, method = "Gauss"
  )
  m_newton <- simulate_scenario(model,
    scenario = "baseline",
    periods = 60, method = "Newton"
  )

  expect_equal(
    m_gauss$baseline$result,
    m_newton$baseline$result,
    tolerance = 1e-4
  )
})

test_that("Gauss stops on NaN or Inf", {
  # division by zero gives Inf in the first period
  broken <- create_model(name = "broken")
  broken <- add_variable(broken, "y", init = 1)
  broken <- add_variable(broken, "z", init = 0)
  broken <- add_equation(broken, "y = y[-1] / z")

  expect_error(
    suppressWarnings(
      simulate_scenario(broken,
        scenario = "baseline",
        periods = 5, method = "Gauss"
      )
    ),
    regexp = "NaN or Inf"
  )
})

test_that("Gauss warns when max_iter is exhausted", {
  model <- create_model(name = "SIM", template = "SIM")

  # with max_iter = 1 the solver cannot converge and should warn
  warns <- character(0)
  res <- tryCatch(
    withCallingHandlers(
      simulate_scenario(model,
        scenario = "baseline",
        periods = 10, method = "Gauss", max_iter = 1, tol = 1e-12
      ),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  expect_true(any(grepl("max_iter", warns)))

  # only the hidden equation verdict is an acceptable error here
  if (inherits(res, "error")) {
    expect_match(conditionMessage(res), "hidden", ignore.case = TRUE)
  }
})

test_that("Gauss verbose prints periods and iterations", {
  model <- create_model(name = "SIM", template = "SIM")

  msgs <- testthat::capture_messages(
    simulate_scenario(model,
      scenario = "baseline",
      periods = 5, method = "Gauss", verbose = TRUE
    )
  )

  expect_true(any(grepl("Period:", msgs)))
  expect_true(any(grepl("Iteration:", msgs)))
})
