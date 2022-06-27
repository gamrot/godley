# model SIMEX

# Create empty model
model_SIMEX <- create_model(name = "SFC SIMEX")

# Add variables
model_SIMEX <- model_SIMEX %>%
  add_variable("C_d", desc = "Consumption demand by households") %>%
  add_variable("C_s", desc = "Consumption supply") %>%
  add_variable("G_s", desc = "Government supply") %>%
  add_variable("T_d", desc = "Taxes, demand") %>%
  add_variable("T_s", desc = "Taxes, supply") %>%
  add_variable("N_d", desc = "Demand for labor") %>%
  add_variable("N_s", desc = "Supply of labor") %>%
  add_variable("H_h", desc = "Cash money held by households") %>%
  add_variable("H_s", desc = "Cash money supplied by the government") %>%
  add_variable("H_d", desc = "Cash money demanded by the government") %>%
  add_variable("Y", desc = "Income = GDP") %>%
  add_variable("Yd", desc = "Disposable income of households") %>%
  add_variable("Yd_e", desc = "Expected disposable income of households") %>%
  add_variable("alpha1", init = 0.6, desc = "Propensity to consume out of income") %>%
  add_variable("alpha2", init = 0.4, desc = "Propensity to consume out of wealth") %>%
  add_variable("theta", init = 0.2, desc = "Tax rate") %>%
  add_variable("G_d", init = 20, desc = "Government demand") %>%
  add_variable("W", init = 1, desc = "Wage rate")


# Add equations
model_SIMEX <- model_SIMEX %>%
  add_equation("C_s = C_d", desc = "Consumption") %>%
  add_equation("G_s = G_d") %>%
  add_equation("T_s = T_d") %>%
  add_equation("N_s = N_d") %>%
  add_equation("Yd = W * N_s - T_s") %>%
  add_equation("T_d = theta * W * N_s") %>%
  add_equation("C_d = alpha1 * Yd_e + alpha2 * H_h[-1]") %>%
  add_equation("H_s = G_d - T_d + H_s[-1]") %>%
  add_equation("H_h = Yd - C_d + H_h[-1]") %>%
  add_equation("Y = C_s + G_s") %>%
  add_equation("N_d = Y/W") %>%
  add_equation("H_d = Yd_e - C_d + H_h[-1]") %>%
  add_equation("Yd[-1] = Yd_e") %>%
  add_equation("H_s = H_h", desc = "Money equilibrium", hidden = TRUE)

# Simulate model
model_SIMEX <- simulate_scenario(model_SIMEX, scenario = "baseline", max_iter = 350, periods = 100, hidden_tol = 0.1, tol = 1e-08, method = "Gauss")

# Create empty shock
shock_SIMEX <- create_shock()

# Add shock equation with increased government expenditures and create new scenario with this shock
shock_SIMEX <- shock_SIMEX %>%
  add_shock(equation = "G_d = 25", desc = "permanent increase in government expenditures", start = 5, end = 50)

model_SIMEX <- model_SIMEX %>%
  add_scenario(name = "expansion", origin = "baseline", origin_period = 1, shock = shock_SIMEX)

# Simulate shock
model_SIMEX <- simulate_scenario(model_SIMEX, scenario = "expansion", max_iter = 350, periods = 50, hidden_tol = 0.1, tol = 1e-08, method = "Newton")

# Plot results
plot_simulation(model = model_SIMEX, scenario = "baseline", from = 1, to = 100, expressions = c("Y", "C_d", "C_s / alpha1"))
