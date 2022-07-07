## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(godley)
model_sim <- create_model(name = "SFC model")

## -----------------------------------------------------------------------------
# add parameters
model_sim <- add_variable(model = model_sim, name = "C_d", desc = "Consumption demand by households")
model_sim <- add_variable(model = model_sim, name = "C_s", desc = "Consumption supply")
model_sim <- add_variable(model = model_sim, name = "G_s", desc = "Government supply")
model_sim <- add_variable(model = model_sim, name = "H_h", desc = "Cash money held by households")
model_sim <- add_variable(
  model = model_sim, name = "H_s",
  desc = "Cash money supplied by the government"
)
model_sim <- add_variable(model = model_sim, name = "N_d", desc = "Demand for labor")
model_sim <- add_variable(model = model_sim, name = "N_s", desc = "Supply of labor")
model_sim <- add_variable(model = model_sim, name = "T_d", desc = "Taxes, demand")
model_sim <- add_variable(model = model_sim, name = "T_s", desc = "Taxes, supply")
model_sim <- add_variable(model = model_sim, name = "Y", desc = "Income = GDP")
model_sim <- add_variable(model = model_sim, name = "Yd", desc = "Disposable income of households")
model_sim <- add_variable(
  model = model_sim, name = "alpha1", init = 0.6,
  desc = "Propensity to consume out of income"
)
model_sim <- add_variable(
  model = model_sim, name = "alpha2", init = 0.4,
  desc = "Propensity to consume out of wealth"
)
model_sim <- add_variable(model = model_sim, name = "theta", init = 0.2, desc = "Tax rate")
model_sim <- add_variable(model = model_sim, name = "G_d", init = 20, desc = "Government demand")
model_sim <- add_variable(model = model_sim, name = "W", init = 1, desc = "Wage rate")

## -----------------------------------------------------------------------------
# add equations
model_sim <- add_equation(model = model_sim, equation = "C_s = C_d", desc = "Consumption")
model_sim <- add_equation(model = model_sim, equation = "G_s = G_d")
model_sim <- add_equation(model = model_sim, equation = "T_s = T_d")
model_sim <- add_equation(model = model_sim, equation = "N_s = N_d")
model_sim <- add_equation(model = model_sim, equation = "Yd = W * N_s - T_s")
model_sim <- add_equation(model = model_sim, equation = "T_d = theta * W * N_s")
model_sim <- add_equation(model = model_sim, equation = "C_d = alpha1 * Yd + alpha2 * H_h[-1]")
model_sim <- add_equation(model = model_sim, equation = "H_s = G_d - T_d + H_s[-1]")
model_sim <- add_equation(model = model_sim, equation = "H_h = Yd - C_d + H_h[-1]")
model_sim <- add_equation(model = model_sim, equation = "Y = C_s + G_s")
model_sim <- add_equation(model = model_sim, equation = "N_d = Y/W")
model_sim <- add_equation(
  model = model_sim, equation = "H_s = H_h", desc = "Money equilibrium",
  hidden = TRUE
)


## -----------------------------------------------------------------------------
# change initial value for alpha1 parameter
model_sim <- change_init(model = model_sim, name = "alpha1", value = 0.5)

## -----------------------------------------------------------------------------
# simulate baseline scenario
model_sim <- simulate_scenario(
  model = model_sim, scenario = "baseline", max_iter = 350, periods = 100,
  hidden_tol = 0.1, tol = 1e-08, method = "Gauss"
)

## ---- out.width="100%"--------------------------------------------------------
# plot results
plot_simulation(
  model = model_sim, scenario = "baseline", from = 1, to = 100,
  expressions = c("Y", "C_s / alpha1")
)

## -----------------------------------------------------------------------------
# create shock
sim_shock <- create_shock()

## -----------------------------------------------------------------------------
# add shock equation for increased government expenditures
sim_shock <- add_shock(
  shock = sim_shock, equation = "G_d = 25",
  desc = "Increase in government expenditures", start = 5, end = 10
)

## -----------------------------------------------------------------------------
# add shock scenario for increased government expenditures
model_sim <- add_scenario(
  model = model_sim, name = "expansion", origin = "baseline",
  origin_period = 100, shock = sim_shock
)

## -----------------------------------------------------------------------------
# calculate shock scenario for increased government expenditures
model_sim <- simulate_scenario(
  model = model_sim, max_iter = 350, periods = 100, hidden_tol = 0.1,
  tol = 1e-08, method = "Gauss"
)

## -----------------------------------------------------------------------------
# create and calculate sensitivity
model_sen <- create_sensitivity(
  model_pass = model_sim, variable = "alpha1",
  lower = 0.1, upper = 0.8, step = 0.1
)

model_sen <- simulate_scenario(
  model = model_sen, max_iter = 350, periods = 100, hidden_tol = 0.1,
  tol = 1e-08, method = "Gauss"
)
