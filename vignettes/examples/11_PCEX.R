# model PCEX - model PC with expectations (random shocks)

# Create empty model
model_pcex <- create_model(name = "SFC PCEX")

# Add variables
model_pcex <- model_pcex |>
  add_variable("B_cb", desc = "") |>
  add_variable("H_s", desc = "") |>
  add_variable("B_s", desc = "") |>
  add_variable("B_h", desc = "") |>
  add_variable("H_d1", desc = "") |>
  add_variable("H_d", desc = "") |>
  add_variable("H_h", desc = "") |>
  add_variable("C", desc = "") |>
  add_variable("V", desc = "") |>
  add_variable("T_x", desc = "") |>
  add_variable("Y", desc = "Income = GDP") |>
  add_variable("Yd", desc = "Disposable income of households") |>
  add_variable("Yd_e") |>
  add_variable("B_d") |>
  add_variable("V_e") |>
  add_variable("Ra") |>
  add_variable("alpha1", init = 0.6, desc = "Propensity to consume out of income") |>
  add_variable("alpha2", init = 0.4, desc = "Propensity to consume out of wealth") |>
  add_variable("theta", init = 0.2, desc = "Tax rate") |>
  add_variable("r", init = 0.025, desc = "") |>
  add_variable("G", init = 20, desc = "Government demand") |>
  add_variable("lambda0", init = 0.635, desc = "") |>
  add_variable("lambda1", init = 0.05, desc = "") |>
  add_variable("lambda2", init = 0.01, desc = "")

# Add equations
model_pcex <- model_pcex |>
  add_equation("Y = C + G", desc = "") |>
  add_equation("Yd = Y - T_x + r[-1] * B_h[-1]") |>
  add_equation("T_x = theta * (Y + r[-1] * B_h[-1])") |>
  add_equation("V = V[-1] + (Yd - C)") |>
  add_equation("C = alpha1 * Yd_e + alpha2 * V[-1]") |>
  add_equation("B_d =  V_e * lambda0 + V_e * lambda1 * r - lambda2 * Yd_e") |>
  add_equation("H_d1 = V_e * (1 - lambda0) - V_e * lambda1 * r + lambda2 * Yd_e") |>
  add_equation("H_d = V_e - B_d") |>
  add_equation("V_e = V[-1] + (Yd_e - C)") |>
  add_equation("H_h = V - B_h") |>
  add_equation("B_h = B_d") |>
  add_equation("B_s = B_s[-1] + (G + r[-1] * B_s[-1]) - (T_x + r[-1] * B_cb[-1])") |>
  add_equation("H_s = H_s[-1] + B_cb - B_cb[-1]") |>
  add_equation("B_cb = B_s - B_h") |>
  add_equation("Yd_e = Yd * (1 + Ra)") |>
  add_equation("Ra = rnorm(1, 0, 0.05)") |>
  add_equation("H_h = H_s", hidden = T)

# Simulate model
model_pcex <- simulate_scenario(model_pcex, scenario = "baseline", max_iter = 350, periods = 100, hidden_tol = 0.1, tol = 1e-05, method = "Newton")

# Plot results
plot_simulation(
  model = model_pcex, scenario = c("baseline"), from = 1, to = 50,
  expressions = c("H_h", 'H_d')
)

# model PCEX1 - model PC with expectations (adaptive expectations)

# Create empty model
model_pcex1 <- create_model(name = "SFC PCEX1")

# Add variables
model_pcex1 <- model_pcex1 |>
  add_variable("B_cb", desc = "") |>
  add_variable("H_s", desc = "") |>
  add_variable("B_s", desc = "") |>
  add_variable("B_h", desc = "") |>
  add_variable("H_d1", desc = "") |>
  add_variable("H_d", desc = "") |>
  add_variable("H_h", desc = "") |>
  add_variable("C", desc = "") |>
  add_variable("V", desc = "") |>
  add_variable("T_x", desc = "") |>
  add_variable("Y", desc = "Income = GDP") |>
  add_variable("Yd", desc = "Disposable income of households") |>
  add_variable("Yd_e") |>
  add_variable("B_d") |>
  add_variable("V_e") |>
  add_variable("alpha1", init = 0.6, desc = "Propensity to consume out of income") |>
  add_variable("alpha2", init = 0.4, desc = "Propensity to consume out of wealth") |>
  add_variable("theta", init = 0.2, desc = "Tax rate") |>
  add_variable("r", init = 0.025, desc = "") |>
  add_variable("G", init = 20, desc = "Government demand") |>
  add_variable("lambda0", init = 0.635, desc = "") |>
  add_variable("lambda1", init = 0.05, desc = "") |>
  add_variable("lambda2", init = 0.01, desc = "")

# Add equations
model_pcex1 <- model_pcex1 |>
  add_equation("Y = C + G", desc = "") |>
  add_equation("Yd = Y - T_x + r[-1] * B_h[-1]") |>
  add_equation("T_x = theta * (Y + r[-1] * B_h[-1])") |>
  add_equation("V = V[-1] + (Yd - C)") |>
  add_equation("C = alpha1 * Yd_e + alpha2 * V[-1]") |>
  add_equation("B_d =  V_e * lambda0 + V_e * lambda1 * r - lambda2 * Yd_e") |>
  add_equation("H_d1 = V_e * (1 - lambda0) - V_e * lambda1 * r + lambda2 * Yd_e") |>
  add_equation("H_d = V_e - B_d") |>
  add_equation("V_e = V[-1] + (Yd_e - C)") |>
  add_equation("H_h = V - B_h") |>
  add_equation("B_h = B_d") |>
  add_equation("B_s = B_s[-1] + (G + r[-1] * B_s[-1]) - (T_x + r[-1] * B_cb[-1])") |>
  add_equation("H_s = H_s[-1] + B_cb - B_cb[-1]") |>
  add_equation("B_cb = B_s - B_h") |>
  add_equation("Yd_e = Yd[-1]") |>
  add_equation("H_h = H_s", hidden = T)

# Simulate model
model_pcex1 <- simulate_scenario(model_pcex1, scenario = "baseline", max_iter = 350, periods = 100, hidden_tol = 0.1, tol = 1e-05, method = "Newton")

# Plot results
plot_simulation(
  model = model_pcex1, scenario = c("baseline"), from = 1, to = 50,
  expressions = c("Y")
)

#shock - increase in the propensity to consume out of disposable income 

# Create empty shock
shock_pcex1 <- create_shock()

# Add shock equation with increased government expenditures
shock_pcex1 <- add_shock(shock_pcex1,
                         equation = "alpha1 = 0.7",
                         desc = "Increase in the propensity to consume out of disposable income", start = 5, end = 50
)

# Create new scenario with this shock
model_pcex1 <- add_scenario(model_pcex1,
                            name = "expansion", origin = "baseline",
                            origin_period = 100, shock = shock_pcex1
)

# Simulate shock
model_pcex1 <- simulate_scenario(model_pcex1,
                                 scenario = "expansion", max_iter = 350, periods = 100,
                                 hidden_tol = 0.1, tol = 1e-05, method = "Newton"
)

# Plot results
plot_simulation(
  model = model_pcex1, scenario = c("expansion"), from = 1, to = 50,
  expressions = c("Y")
)

# Plot results
plot_simulation(
  model = model_pcex1, scenario = c("expansion"), from = 1, to = 50,
  expressions = c("Yd_e", "C", "V")
)

# model PCEX2 - model PC with expectations (where the propensity to consume reacts negatively to higher interest rates)

# Create empty model
model_pcex2 <- create_model(name = "SFC PCEX2")

# Add variables
model_pcex2 <- model_pcex2 |>
  add_variable("B_cb", desc = "") |>
  add_variable("H_s", desc = "") |>
  add_variable("B_s", desc = "") |>
  add_variable("B_h", desc = "") |>
  add_variable("H_d1", desc = "") |>
  add_variable("H_d", desc = "") |>
  add_variable("H_h", desc = "") |>
  add_variable("C", desc = "") |>
  add_variable("V", desc = "") |>
  add_variable("T_x", desc = "") |>
  add_variable("Y", desc = "Income = GDP") |>
  add_variable("Yd", desc = "Disposable income of households") |>
  add_variable("Yd_e") |>
  add_variable("B_d") |>
  add_variable("V_e") |>
  add_variable("iota", init = 4) |>
  add_variable("alpha10", init = 0.7) |>
  add_variable("alpha1", init = 0.6, desc = "Propensity to consume out of income") |>
  add_variable("alpha2", init = 0.4, desc = "Propensity to consume out of wealth") |>
  add_variable("theta", init = 0.2, desc = "Tax rate") |>
  add_variable("r", init = 0.025, desc = "") |>
  add_variable("G", init = 20, desc = "Government demand") |>
  add_variable("lambda0", init = 0.635, desc = "") |>
  add_variable("lambda1", init = 0.05, desc = "") |>
  add_variable("lambda2", init = 0.01, desc = "")

# Add equations
model_pcex2 <- model_pcex2 |>
  add_equation("Y = C + G", desc = "") |>
  add_equation("Yd = Y - T_x + r[-1] * B_h[-1]") |>
  add_equation("T_x = theta * (Y + r[-1] * B_h[-1])") |>
  add_equation("V = V[-1] + (Yd - C)") |>
  add_equation("C = alpha1 * Yd_e + alpha2 * V[-1]") |>
  add_equation("B_d =  V_e * lambda0 + V_e * lambda1 * r - lambda2 * Yd_e") |>
  add_equation("H_d1 = V_e * (1 - lambda0) - V_e * lambda1 * r + lambda2 * Yd_e") |>
  add_equation("H_d = V_e - B_d") |>
  add_equation("V_e = V[-1] + (Yd_e - C)") |>
  add_equation("H_h = V - B_h") |>
  add_equation("B_h = B_d") |>
  add_equation("B_s = B_s[-1] + (G + r[-1] * B_s[-1]) - (T_x + r[-1] * B_cb[-1])") |>
  add_equation("H_s = H_s[-1] + B_cb - B_cb[-1]") |>
  add_equation("B_cb = B_s - B_h") |>
  add_equation("Yd_e = Yd[-1]") |>
  add_equation("alpha1 = alpha10 - iota * r[-1]") |>
  add_equation("H_h = H_s", hidden = T)

# Simulate model
model_pcex2 <- simulate_scenario(model_pcex2, scenario = "baseline", max_iter = 350, periods = 100, hidden_tol = 0.1, tol = 1e-05, method = "Newton")

# Plot results
plot_simulation(
  model = model_pcex2, scenario = c("baseline"), from = 1, to = 50,
  expressions = c("Y")
)

#shock - increase in the rate of interest on bills

# Create empty shock
shock_pcex2 <- create_shock()

# Add shock equation with increased government expenditures
shock_pcex2 <- add_shock(shock_pcex2,
                         equation = "r = 0.035",
                         desc = "Increase in the rate of interest on bills", start = 5, end = 50
)

# Create new scenario with this shock
model_pcex2 <- add_scenario(model_pcex2,
                            name = "expansion", origin = "baseline",
                            origin_period = 100, shock = shock_pcex2
)

# Simulate shock
model_pcex2 <- simulate_scenario(model_pcex2,
                                 scenario = "expansion", max_iter = 350, periods = 100,
                                 hidden_tol = 0.1, tol = 1e-05, method = "Newton"
)

# Plot results
plot_simulation(
  model = model_pcex2, scenario = c("expansion"), from = 1, to = 50,
  expressions = c("Y")
)

# Plot results
plot_simulation(
  model = model_pcex2, scenario = c("expansion"), from = 1, to = 50,
  expressions = c("Yd", "C", "V")
)
