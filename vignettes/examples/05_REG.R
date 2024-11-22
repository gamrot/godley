# model REG

# Create empty model
model_reg <- create_model(name = "SFC REG")

# Add variables
model_reg <- model_reg |>
  add_variable("r", init = 0.025) |>
  add_variable("G_S", init = 20) |>
  add_variable("G_N", init = 20) |>
  add_variable("mu_N", init = 0.15) |>
  add_variable("mu_S", init = 0.15) |>
  add_variable("alpha1_N", init = 0.7) |>
  add_variable("alpha1_S", init = 0.7) |>
  add_variable("alpha2_N", init = 0.3) |>
  add_variable("alpha2_S", init = 0.3) |>
  add_variable("lambda0_N", init = 0.67) |>
  add_variable("lambda0_S", init = 0.67) |>
  add_variable("lambda1_N", init = 0.05) |>
  add_variable("lambda1_S", init = 0.05) |>
  add_variable("lambda2_N", init = 0.01) |>
  add_variable("lambda2_S", init = 0.01) |>
  add_variable("theta", init = 0.2) |>
  add_variable("Y_N") |>
  add_variable("C_N") |>
  add_variable("X_N") |>
  add_variable("IM_N") |>
  add_variable("Y_S") |>
  add_variable("C_S") |>
  add_variable("X_S") |>
  add_variable("IM_S") |>
  add_variable("YD_N") |>
  add_variable("TX_N") |>
  add_variable("Bh_N") |>
  add_variable("YD_S") |>
  add_variable("TX_S") |>
  add_variable("Bh_S") |>
  add_variable("V_N") |>
  add_variable("V_S") |>
  add_variable("Hh_N") |>
  add_variable("Hh_S") |>
  add_variable("TX") |>
  add_variable("G") |>
  add_variable("Bh") |>
  add_variable("Bs") |>
  add_variable("Hh") |>
  add_variable("Hs") |>
  add_variable("Bcb")

# Add equations
model_reg <- model_reg |>
  add_equation("Y_N = C_N + G_N + X_N - IM_N") |>
  add_equation("Y_S = C_S + G_S + X_S - IM_S") |>
  add_equation("IM_N = mu_N * Y_N") |>
  add_equation("IM_S = mu_S * Y_S") |>
  add_equation("X_N = IM_S") |>
  add_equation("YD_N = Y_N - TX_N + r[-1] * Bh_N[-1]") |>
  add_equation("YD_S = Y_S - TX_S + r[-1] * Bh_S[-1]") |>
  add_equation("TX_N = theta * ( Y_N + r[-1] * Bh_N[-1])") |>
  add_equation("X_S = IM_N") |>
  add_equation("TX_S = theta * ( Y_S + r[-1] * Bh_S[-1])") |>
  add_equation("V_N = V_N[-1] + ( YD_N - C_N )") |>
  add_equation("V_S = V_S[-1] + ( YD_S - C_S )") |>
  add_equation("C_N = alpha1_N * YD_N + alpha2_N * V_N[-1]") |>
  add_equation("C_S = alpha1_S * YD_S + alpha2_S * V_S[-1]") |>
  add_equation("Hh_N = V_N - Bh_N") |>
  add_equation("Hh_S = V_S - Bh_S") |>
  add_equation("Bh_N = V_N * ( lambda0_N + lambda1_N * r - lambda2_N * ( YD_N/V_N ) )") |>
  add_equation("Bh_S = V_S * ( lambda0_S + lambda1_S * r - lambda2_S * ( YD_S/V_S ) )") |>
  add_equation("TX = TX_N + TX_S") |>
  add_equation("G = G_N + G_S") |>
  add_equation("Bh = Bh_N + Bh_S") |>
  add_equation("Hh = Hh_N + Hh_S") |>
  add_equation("Bs = Bs[-1] + ( G + r[-1] * Bs[-1] ) - ( TX + r[-1] * Bcb[-1] )") |>
  add_equation("Hs = Hs[-1] + Bcb - Bcb[-1]") |>
  add_equation("Bcb = Bs - Bh") |>
  add_equation("Hs = Hh", desc = "Money equilibrium", hidden = TRUE)

# Simulate model
model_reg <- simulate_scenario(model_reg,
  scenario = "baseline", max_iter = 350, periods = 100,
  hidden_tol = 0.1, tol = 1e-05, method = "Gauss"
)

# Plot results
plot_simulation(
  model = model_reg, scenario = "baseline", from = 1, to = 60,
  expressions = c(
    "deltaV_S = V_S - dplyr::lag(V_S)",
    "GB_S = TX_S - (G_S + dplyr::lag(r) * dplyr::lag(Bh_S))",
    "TB_S = X_S - IM_S"
  )
)

# Scenario 1: An increase in the propensity to import of the South

shock <- create_shock() |>
  add_shock(variable="mu_S", value=0.25, desc = "An increase in the propensity to import of the South", start = 5, end = 60)

model_reg <- model_reg |>
  add_scenario(name = "expansion", origin = "baseline", origin_start = 1, origin_end = 1, shock = shock)

model_reg <- simulate_scenario(model_reg, scenario = "expansion", max_iter = 350, periods = 60, hidden_tol = 0.1, tol = 1e-05, method = "Gauss")

plot_simulation(
  model = model_reg, scenario = "expansion", from = 1, to = 60,
  expressions = c("deltaV_S = V_S - dplyr::lag(V_S)", "GB_S = TX_S - (G_S + dplyr::lag(r) * dplyr::lag(Bh_S))", "TB_S = X_S - IM_S")
)

plot_simulation(
  model = model_reg, scenario = "expansion", from = 1, to = 60,
  expressions = c("Y_N", "Y_S")
)

# Scenario 2: An increase in the government expenditures of the South

shock2 <- create_shock() |>
  add_shock(variable = "G_S", value=25, desc = "An increase in the government expenditures of the South", start = 5, end = 60)

model_reg <- model_reg |>
  add_scenario(name = "expansion2", origin = "baseline", origin_start=1, origin_end=100, shock = shock2)

model_reg <- simulate_scenario(model_reg, scenario = "expansion2", max_iter = 350, periods = 60, hidden_tol = 0.1, tol = 1e-05, method = "Gauss")

plot_simulation(model_reg,
  scenario = "expansion2", from = 1, to = 60,
  expressions = c("Y_N", "Y_S")
)

plot_simulation(model_reg,
  scenario = "expansion2", from = 1, to = 60,
  expressions = c("deltaV_S = V_S - dplyr::lag(V_S)", "GB_S = TX_S - (G_S + dplyr::lag(r) * dplyr::lag(Bh_S))", "TB_S = X_S - IM_S")
)

# Scenario 3: An increase in the propensity to save of the Southern households

shock3 <- create_shock() |>
  add_shock(variable = "alpha1_S", value=.6, desc = "", start = 5, end = 60)

model_reg <- model_reg |>
  add_scenario(name = "expansion3", origin = "baseline", origin_start = 1, origin_end = 100, shock = shock3)

model_reg <- simulate_scenario(model_reg, scenario = "expansion3", max_iter = 350, periods = 60, hidden_tol = 0.1, tol = 1e-05, method = "Gauss")

plot_simulation(model_reg,
  scenario = "expansion3", from = 1, to = 60,
  expressions = c("Y_N", "Y_S")
)

plot_simulation(model_reg,
  scenario = "expansion3", from = 1, to = 60,
  expressions = c("deltaV_S = V_S - dplyr::lag(V_S)", "GB_S = TX_S - (G_S + dplyr::lag(r) * dplyr::lag(Bh_S))", "TB_S = X_S - IM_S")
)

