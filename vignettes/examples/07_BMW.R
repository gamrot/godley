# model BMW

# Create empty model
model_bmw <- create_model(name = "SFC BMW")

# Add variables
model_bmw <- model_bmw |>
  add_variable("rl", init = 0.025) |>
  add_variable("alpha0", init = 20) |>
  add_variable("alpha1", init = 0.75) |>
  add_variable("alpha2", init = 0.10) |>
  add_variable("delta", init = 0.10) |>
  add_variable("gamma", init = 0.15) |>
  add_variable("kappa", init = 1) |>
  add_variable("pr", init = 1) |>
  add_variable("Nd", init = .001) |>
  add_variable("Ns", init = .001) |>
  add_variable("Y", init = .001) |>
  add_variable("Cs") |>
  add_variable("Cd") |>
  add_variable("Is") |>
  add_variable("Id") |>
  add_variable("Ls") |>
  add_variable("Ld") |>
  add_variable("WBd") |>
  add_variable("AF") |>
  add_variable("K") |>
  add_variable("YD") |>
  add_variable("WBs") |>
  add_variable("rm") |>
  add_variable("Mh") |>
  add_variable("Ms") |>
  add_variable("W") |>
  add_variable("DA") |>
  add_variable("KT")

# Add equations
model_bmw <- model_bmw |>
  add_equation("Cs = Cd") |>
  add_equation("Is = Id") |>
  add_equation("Ns = Nd") |>
  add_equation("Ls = Ls[-1] + Ld - Ld[-1]") |>
  add_equation("Y = Cs + Is") |>
  add_equation("WBd = Y - rl[-1] * Ld[-1] - AF") |>
  add_equation("AF = delta * K[-1]") |>
  add_equation("Ld = Ld[-1] + Id - AF") |>
  add_equation("YD = WBs + rm[-1] * Mh[-1]") |>
  add_equation("Mh = Mh[-1] + YD - Cd") |>
  add_equation("Ms = Ms[-1] + Ls - Ls[-1]") |>
  add_equation("rm = rl") |>
  add_equation("WBs = W * Ns") |>
  add_equation("Nd = Y / pr") |>
  add_equation("W = WBd / Nd") |>
  add_equation("Cd = alpha0 + alpha1 * YD + alpha2 * Mh[-1]") |>
  add_equation("K = K[-1] + Id - DA") |>
  add_equation("DA = delta * K[-1]") |>
  add_equation("KT = kappa * Y[-1]") |>
  add_equation("Id = gamma * (KT - K[-1]) + DA") |>
  add_equation("Ms = Mh", hidden = T)

# Simulate model
model_bmw <- simulate_scenario(model_bmw, scenario = "baseline", max_iter = 350, periods = 100, hidden_tol = 0.1, tol = 1e-05, method = "Newton")

# Plot results
plot_simulation(model = model_bmw, scenario = "baseline", from = 1, to = 50, expressions = c("Y"))

# Scenario 1: Increase in autonomous consumption expenditures
shock <- create_shock() |>
  add_shock(
    variable = "alpha0",
    value = 30,
    desc = "Increase in autonomous consumption expenditures",
    start = 5,
    end = 50
    )

model_bmw <- model_bmw |>
  add_scenario(
    name = "expansion",
    origin = "baseline",
    origin_start = 1,
    origin_end = 100, 
    shock = shock
    )

model_bmw <- simulate_scenario(model_bmw, scenario = "expansion", max_iter = 350, periods = 100, hidden_tol = 10, tol = 1e-05, method = "Newton")

plot_simulation(
  model = model_bmw, scenario = "expansion", from = 1, to = 50,
  expressions = c("Cd", "YD")
)

plot_simulation(
  model = model_bmw, scenario = "expansion", from = 1, to = 50,
  expressions = c("Id", "AF")
)

# Scenario 2: Increase in the propensity to save
shock2 <- create_shock() |>
  add_shock(variable = "alpha1", value=0.7, desc = "Increase in the propensity to save", start = 5, end = 50)

model_bmw <- model_bmw |>
  add_scenario(
    name = "expansion2",
    origin = "baseline",
    origin_start = 1,
    origin_end = 100, 
    shock = shock2
    )

model_bmw <- simulate_scenario(
  model_bmw,
  scenario = "expansion2",
  max_iter = 350,
  periods = 100,
  hidden_tol = .1,
  tol = 1e-05,
  method = "Newton"
  )

plot_simulation(
  model = model_bmw, scenario = "expansion2", from = 1, to = 50,
  expressions = c("Cd", "YD")
)

plot_simulation(
  model = model_bmw, scenario = "expansion2", from = 1, to = 50,
  expressions = c("YK = Y / dplyr::lag(K)")
)

