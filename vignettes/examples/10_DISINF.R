# model DISINF

# Create empty model
model_disinf <- create_model(name = "SFC DISINF")

# Add variables
model_disinf <- model_disinf |>
  add_variable("rrc", init = 0.025) |>
  add_variable("pr", init = 1) |>
  add_variable("add", init = 0.02) |>
  add_variable("alpha0", init = 15) |>
  add_variable("alpha1", init = 0.8) |>
  add_variable("alpha2", init = 0.1) |>
  add_variable("beta", init = 0.9) |>
  add_variable("epsilon", init = 0.8) |>
  add_variable("gamma", init = 0.25) |>
  add_variable("phi", init = 0.24) |>
  add_variable("sigma_T", init = 0.2) |>
  add_variable("Omega0", init = -1.4) |>
  add_variable("Omega1", init = 1) |>
  add_variable("Omega2", init = 1.2) |>
  add_variable("Omega3", init = 0.3) |>
  add_variable("p", init = 1) |>
  add_variable("W", init = 1) |>
  add_variable("UC", init = 1) |>
  add_variable("s_E", init = .00001) |>
  add_variable("inv_T") |>
  add_variable("inv_E") |>
  add_variable("inv") |>
  add_variable("s") |>
  add_variable("c") |>
  add_variable("N") |>
  add_variable("WB") |>
  add_variable("INV") |>
  add_variable("S") |>
  add_variable("EF") |>
  add_variable("Ld") |>
  add_variable("Ls") |>
  add_variable("Ms") |>
  add_variable("rm") |>
  add_variable("EFb") |>
  add_variable("Mh") |>
  add_variable("YD") |>
  add_variable("C") |>
  add_variable("omega_T") |>
  add_variable("Nfe") |>
  add_variable("yfe") |>
  add_variable("mh") |>
  add_variable("y") |>
  add_variable("rl") |>
  add_variable("pic") |>
  add_variable("ydhs") |>
  add_variable("yd") |>
  add_variable("ydhs_E")

# Add equations
model_disinf <- model_disinf |>
  add_equation("y = s_E + inv_E - inv[-1]") |>
  add_equation("inv_T = sigma_T * s_E") |>
  add_equation("inv_E = inv[-1] + gamma * (inv_T - inv[-1])") |>
  add_equation("inv = inv[-1] + (y - s)") |>
  add_equation("s_E = beta * s[-1] + (1 - beta) * s_E[-1]") |>
  add_equation("s = c") |>
  add_equation("N = y / pr") |>
  add_equation("WB = N * W") |>
  add_equation("UC = WB / y") |>
  add_equation("INV = inv * UC") |>
  add_equation("S = p * s") |>
  add_equation("p = (1 + phi) * (1 + rrc * sigma_T) * UC") |>
  add_equation("EF = S - WB + (INV - INV[-1]) - rl * INV[-1]") |>
  add_equation("Ld = INV") |>
  add_equation("Ls = Ld") |>
  add_equation("Ms = Ls") |>
  add_equation("rm = rl - add") |>
  add_equation("EFb = rl[-1] * Ls[-1] - rm[-1] * Mh[-1]") |>
  add_equation("pic = (UC / UC[-1]) - 1") |>
  add_equation("rl = (1 + rrc) * (1 + pic) - 1") |>
  add_equation("YD = WB + EF + EFb + rm * Mh[-1]") |>
  add_equation("Mh = Mh[-1] + YD - C") |>
  add_equation("ydhs = c + (mh - mh[-1])") |>
  add_equation("yd = YD / p") |>
  add_equation("C = c * p") |>
  add_equation("mh = Mh / p") |>
  add_equation("c = alpha0 + alpha1 * ydhs_E + alpha2 * mh[-1]") |>
  add_equation("ydhs_E = epsilon * ydhs[-1] + (1 - epsilon) * ydhs_E[-1]") |>
  add_equation("omega_T = Omega0 + Omega1 * pr + Omega2 * (N / Nfe)") |>
  add_equation("W = W[-1] * (1 + Omega3 * (omega_T[-1] - (W[-1]/p[-1])))") |>
  add_equation("yfe = (1 + sigma_T) * s - inv[-1]") |>
  add_equation("Nfe = s / pr")

# Simulate model
model_disinf <- simulate_scenario(model_disinf, scenario = "baseline", max_iter = 350, periods = 100, hidden_tol = 0.1, tol = 1e-05, method = "Gauss")

# Plot results
plot_simulation(model = model_disinf, scenario = "baseline", from = 1, to = 100, expressions = c("y"))

# Scenario 1: Increase in the costing margins
shock <- create_shock() |>
  add_shock(variable = "phi", value = 0.3, desc = "Increase in the costing margins", start = 5, end = 100)

model_disinf <- model_disinf |>
  add_scenario(name = "expansion", origin = "baseline", origin_start=1, origin_end=100, shock = shock)

model_disinf <- simulate_scenario(model_disinf, scenario = "expansion", max_iter = 350, periods = 100, hidden_tol = .1, tol = 1e-05, method = "Gauss")

plot_simulation(
  model = model_disinf, scenario = "expansion", from = 1, to = 100,
  expressions = c("inflation = (p - dplyr::lag(p)) / dplyr::lag(p)")
)

plot_simulation(
  model = model_disinf, scenario = "expansion", from = 1, to = 100,
  expressions = c("p", "UC", "UCp = UC/p")
)

plot_simulation(
  model = model_disinf, scenario = "expansion", from = 1, to = 100,
  expressions = c("ydhs_ss = alpha0 / (1 - alpha1 - alpha2*sigma_T * (UC/p))", "ydhs", "c", "s")
)

# Scenario 2: Increase in the target real wage
shock2 <- create_shock() |>
  add_shock(variable = "Omega0", value= -1, desc = "Increase in the target real wage", start = 5, end = 100)

model_disinf <- model_disinf |>
  add_scenario(name = "expansion2", origin = "baseline", origin_start=1, origin_end=100, shock = shock2)

model_disinf <- simulate_scenario(model_disinf, scenario = "expansion2", max_iter = 350, periods = 100, tol = 1e-05, method = "Gauss")

plot_simulation(
  model = model_disinf, scenario = "expansion2", from = 1, to = 100,
  expressions = c("ydhs_ss = alpha0 / (1 - alpha1 - alpha2*sigma_T * (UC/p))", "ydhs", "c", "s")
)

