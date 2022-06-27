# model SIM

# Create empty model
model_SIM <- create_model(name = "SFC SIM")

# Add variables
model_SIM <- model_SIM %>%
  add_variable("C_d", desc = "Consumption demand by households") %>%
  add_variable("C_s", desc = "Consumption supply") %>%
  add_variable("G_s", desc = "Government supply") %>%
  add_variable("H_h", desc = "Cash money held by households") %>%
  add_variable("H_s", desc = "Cash money supplied by the government") %>%
  add_variable("N_d", desc = "Demand for labor") %>%
  add_variable("N_s", desc = "Supply of labor") %>%
  add_variable("T_d", desc = "Taxes, demand") %>%
  add_variable("T_s", desc = "Taxes, supply") %>%
  add_variable("Y", desc = "Income = GDP") %>%
  add_variable("Yd", desc = "Disposable income of households") %>%
  add_variable("alpha1", init = 0.6, desc = "Propensity to consume out of income") %>%
  add_variable("alpha2", init = 0.4, desc = "Propensity to consume out of wealth") %>%
  add_variable("theta", init = 0.2, desc = "Tax rate") %>%
  add_variable("G_d", init = 20, desc = "Government demand") %>%
  add_variable("W", init = 1, desc = "Wage rate")

# Add equations
model_SIM <- model_SIM %>%
  add_equation("C_s = C_d", desc = "Consumption") %>%
  add_equation("G_s = G_d") %>%
  add_equation("T_s = T_d") %>%
  add_equation("N_s = N_d") %>%
  add_equation("Yd = W * N_s - T_s") %>%
  add_equation("T_d = theta * W * N_s") %>%
  add_equation("C_d = alpha1 * Yd + alpha2 * H_h[-1]") %>%
  add_equation("H_s = G_d - T_d + H_s[-1]") %>%
  add_equation("H_h = Yd - C_d + H_h[-1]") %>%
  add_equation("Y = C_s + G_s") %>%
  add_equation("N_d = Y/W") %>%
  add_equation("H_s = H_h", desc = "Money equilibrium", hidden = TRUE)

# Simulate model
model_SIM <- simulate_scenario(model_SIM, scenario = "baseline", max_iter = 350, periods = 100, hidden_tol = 0.1, tol = 1e-08, method = "Newton")

# Create empty shock
shock_sim <- create_shock()

# Add shock equation with increased government expenditures and create new scenario with this shock
shock_sim <- shock_sim %>%
  add_shock(equation = "G_d = 25", desc = "Increase in government expenditures", start = 5, end = 50)

model_SIM <- model_SIM %>%
  add_scenario(name = "expansion", origin = "baseline", origin_period = 100, shock = shock_sim)

# Simulate shock
model_SIM <- simulate_scenario(model_SIM,
  scenario = "expansion", max_iter = 350, periods = 100,
  hidden_tol = 0.1, tol = 1e-08, method = "Newton"
)

# Plot results
plot_simulation(model = model_SIM, scenario = c("baseline", "expansion"), from = 1, to = 50, expressions = c("Y", "C_d", "C_s / alpha1"))

# Create sensitivity scenarios for alpha1
model_sen <- create_sensitivity(model_SIM, variable = "alpha1", lower = 0, upper = 0.8, step = 0.1)

# Simulate sensitivity for alpha1
model_sen <- simulate_scenario(model_sen, max_iter = 350, periods = 100, hidden_tol = 0.1, tol = 1e-08, method = "Newton")

# plot sensitivity results for alpha1
plot_simulation(model = model_sen, scenario = "sensitivity", take_all = T, from = 1, to = 100, expressions = c("Y"))
plot_simulation(model = model_sen, scenario = "sensitivity", take_all = T, from = 1, to = 100, expressions = c("C_d"))
