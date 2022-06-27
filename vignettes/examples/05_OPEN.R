# model OPEN

model_OPEN <- create_model(name = "SFC OPEN")

# Add variables
model_OPEN <- model_OPEN %>%
  add_variable("xr", init = 1) %>%
  add_variable("pg_N", init = 1) %>%
  add_variable("r_N", init = 0.025) %>%
  add_variable("r_S", init = 0.025) %>%
  add_variable("G_S", init = 20) %>%
  add_variable("G_N", init = 20) %>%
  add_variable("mu_N", init = 0.15) %>%
  add_variable("mu_S", init = 0.15) %>%
  add_variable("alpha1_N", init = 0.7) %>%
  add_variable("alpha1_S", init = 0.8) %>%
  add_variable("alpha2_N", init = 0.3) %>%
  add_variable("alpha2_S", init = 0.2) %>%
  add_variable("lambda0_N", init = 0.67) %>%
  add_variable("lambda0_S", init = 0.67) %>%
  add_variable("lambda1_N", init = 0.05) %>%
  add_variable("lambda1_S", init = 0.05) %>%
  add_variable("lambda2_N", init = 0.01) %>%
  add_variable("lambda2_S", init = 0.01) %>%
  add_variable("theta_N", init = 0.2) %>%
  add_variable("theta_S", init = 0.2) %>%
  add_variable("Y_N") %>%
  add_variable("Y_S") %>%
  add_variable("C_N") %>%
  add_variable("X_N") %>%
  add_variable("IM_N") %>%
  add_variable("C_S") %>%
  add_variable("X_S") %>%
  add_variable("IM_S") %>%
  add_variable("YD_N") %>%
  add_variable("YD_S") %>%
  add_variable("TX_S") %>%
  add_variable("TX_N") %>%
  add_variable("Bh_S") %>%
  add_variable("Bh_N") %>%
  add_variable("V_N") %>%
  add_variable("V_S") %>%
  add_variable("Hh_N") %>%
  add_variable("Hh_S") %>%
  add_variable("Bs_N") %>%
  add_variable("Bs_S") %>%
  add_variable("Bcb_N") %>%
  add_variable("Bcb_S") %>%
  add_variable("or_N") %>%
  add_variable("or_S") %>%
  add_variable("Hs_N") %>%
  add_variable("Hs_S") %>%
  add_variable("pg_S") %>%
  add_variable("deltaor_S") %>%
  add_variable("deltaor_N")

# Add equations
model_OPEN <- model_OPEN %>%
  add_equation("Y_N = C_N + G_N + X_N - IM_N") %>%
  add_equation("Y_S = C_S + G_S + X_S - IM_S") %>%
  add_equation("IM_N = mu_N * Y_N") %>%
  add_equation("IM_S = mu_S * Y_S") %>%
  add_equation("X_N = IM_S / xr") %>%
  add_equation("X_S = IM_N * xr") %>%
  add_equation("YD_N = Y_N - TX_N + r_N[-1] * Bh_N[-1]") %>%
  add_equation("YD_S = Y_S - TX_S + r_S[-1] * Bh_S[-1]") %>%
  add_equation("TX_N = theta_N * ( Y_N + r_N[-1] * Bh_N[-1])") %>%
  add_equation("TX_S = theta_S * ( Y_S + r_S[-1] * Bh_S[-1])") %>%
  add_equation("V_N = V_N[-1] + ( YD_N - C_N )") %>%
  add_equation("V_S = V_S[-1] + ( YD_S - C_S )") %>%
  add_equation("C_N = alpha1_N * YD_N + alpha2_N * V_N[-1]") %>%
  add_equation("C_S = alpha1_S * YD_S + alpha2_S * V_S[-1]") %>%
  add_equation("Hh_N = V_N - Bh_N") %>%
  add_equation("Hh_S = V_S - Bh_S") %>%
  add_equation("Bh_N = V_N * ( lambda0_N + lambda1_N * r_N - lambda2_N * ( YD_N/V_N ) )") %>%
  add_equation("Bh_S = V_S * ( lambda0_S + lambda1_S * r_S - lambda2_S * ( YD_S/V_S ) )") %>%
  add_equation("Bs_N = Bs_N[-1] + ( G_N + r_N[-1] * Bs_N[-1] ) - ( TX_N + r_N[-1] * Bcb_N[-1] )") %>%
  add_equation("Bs_S = Bs_S[-1] + ( G_S + r_S[-1] * Bs_S[-1] ) - ( TX_S + r_S[-1] * Bcb_S[-1] )") %>%
  add_equation("Bcb_N = Bs_N - Bh_N") %>%
  add_equation("Bcb_S = Bs_S - Bh_S") %>%
  add_equation("or_N = or_N[-1] + (( Hs_N - Hs_N[-1] - ( Bcb_N - Bcb_N[-1] ) )/pg_N)") %>%
  add_equation("or_S = or_S[-1] + (( Hs_S - Hs_S[-1] - ( Bcb_S - Bcb_S[-1] ) )/pg_S)") %>%
  add_equation("Hs_N = Hh_N") %>%
  add_equation("Hs_S = Hh_S") %>%
  add_equation("pg_S = pg_N * xr") %>%
  add_equation("deltaor_S = or_S - or_S[-1]") %>%
  add_equation("deltaor_N = - (or_N - or_N[-1])") %>%
  add_equation("deltaor_S = deltaor_N", hidden = TRUE)

# Simulate model
model_OPEN <- simulate_scenario(model_OPEN, scenario = "baseline", max_iter = 350, periods = 100, hidden_tol = 0.1, tol = 1e-08, method = "Gauss")

plot_simulation(
  model = model_OPEN, scenario = "baseline", from = 1, to = 60,
  expressions = c("TB_N = X_N - IM_N", "TB_S = X_S - IM_S", "GB_N = TX_N - (G_N + dplyr::lag(r_N) * dplyr::lag(Bh_N))", "GB_S = TX_S - (G_S + dplyr::lag(r_S) * dplyr::lag(Bh_S))")
)

# Scenario: Ever-falling gold reserves

shock <- create_shock() %>%
  add_shock(equation = "mu_S = 0.25", desc = "increase in the propensity to import in the South", start = 5, end = 60)

model_OPEN <- model_OPEN %>%
  add_scenario(name = "expansion", origin = "baseline", origin_period = 100, shock = shock)

model_OPEN <- simulate_scenario(model_OPEN, scenario = "expansion", max_iter = 350, periods = 60, hidden_tol = 0.1, tol = 1e-08, method = "Gauss")

plot_simulation(
  model = model_OPEN, scenario = "expansion", from = 1, to = 60,
  expressions = c("Y_N", "Y_S")
)

plot_simulation(
  model = model_OPEN, scenario = "expansion", from = 1, to = 60,
  expressions = c("TB_N = X_N - IM_N", "TB_S = X_S - IM_S", "GB_N = TX_N - (G_N + dplyr::lag(r_N) * dplyr::lag(Bh_N))", "GB_S = TX_S - (G_S + dplyr::lag(r_S) * dplyr::lag(Bh_S))")
)

plot_simulation(
  model = model_OPEN, scenario = "expansion", from = 1, to = 60,
  expressions = c("TB_N = X_N - IM_N", "TB_S = X_S - IM_S", "GB_N = TX_N - (G_N + dplyr::lag(r_N) * dplyr::lag(Bh_N))", "GB_S = TX_S - (G_S + dplyr::lag(r_S) * dplyr::lag(Bh_S))")
)

plot_simulation(
  model = model_OPEN, scenario = "expansion", from = 1, to = 60,
  expressions = c("or_S", "or_N")
)

plot_simulation(
  model = model_OPEN, scenario = "expansion", from = 1, to = 60,
  expressions = c("TB_N = X_N - IM_N", "TB_S = X_S - IM_S", "GB_N = TX_N - (G_N + dplyr::lag(r_N) * dplyr::lag(Bh_N))", "GB_S = TX_S - (G_S + dplyr::lag(r_S) * dplyr::lag(Bh_S))")
)
