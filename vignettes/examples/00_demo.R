
# diagnostics -------------------------------------------------------------

diagnostics(models = "SIM")
diagnostics(models = "PC")

# SIM ---------------------------------------------------------------------

# create SIM model from template
model_sim <- create_model(name = "SFC SIM demo", template = "SIM")

# change default values
model_sim <- model_sim |>
  change_init(name = "alpha1", value = 0.7) |>
  change_init("alpha2", 0.3)

# prepare and simulate baseline scenario
model_sim <- model_sim |>
  simulate_scenario(
    scenario = "baseline",
    max_iter = 350,
    periods = 100,
    hidden_tol = 0.1,
    tol = 1e-05,
    method = "Newton"
  )

# add shock — a permanent increase in government expenditures
shock <- create_shock() |>
  add_shock(
    variable = "G_d",
    value = 25,
    desc = "permanent increase in government expenditures",
    start = 5,
    end = 50
  ) |>
  add_shock(
    variable = "alpha1",
    value = 0.3,
    desc = "",
    start = 5,
    end = NA
  )

model_sim <- model_sim |>
  add_scenario(
    name = "expansion",
    origin = "baseline",
    origin_start = 1,
    shock = shock
  ) |>
  simulate_scenario(
    scenario = "expansion",
    max_iter = 350,
    periods = 100,
    hidden_tol = 0.1,
    tol = 1e-05,
    method = "Newton"
  )

# plot results
plot_simulation(
  model = model_sim,
  scenario = c("baseline", "expansion"),
  take_all = F,
  from = 10,
  to = 60,
  expressions = c("Y", "C_d", "C_s / alpha1")
)

# sensitivity
model_sen <- create_sensitivity(model_sim,
  variable = "alpha1",
  lower = 0,
  upper = 0.8,
  step = 0.1
)

# showcase - function simulates all unsimulated scenarios
model_sen <- model_sen |> simulate_scenario()

# showcase - function plots multiple scenarios and multiple variables and can
# also take all scenarios with name containing given string - useful for sensitivity
plot_simulation(
  model = model_sen,
  scenario = "sensitivity",
  take_all = T,
  from = 1,
  to = 100,
  expressions = c("Y", "C_d")
)

# ERRORS AND WARNINGS ---------------------------------------------------------------------

model_sim <- create_model(name = "SFC SIM demo", template = "SIM")

# WARNING - Duplicated variables in the model

model_sim$equations

model_sim <- model_sim %>%
  add_equation("Y = C_s + T_d")

model_sim <- prepare(model_sim)

# WARNING - A variable not defined

model_sim$variables
model_sim <- model_sim %>%
  add_equation("X = C_s + G_s + gamma")

model_sim <- prepare(model_sim)

# ERROR - changing value of endogenous variable
model_sim <- model_sim |>
  change_init(name = "Y", value = 100)


# ERROR - initial matrix is empty

model_sim <- model_sim |>
  add_scenario(
    name = "expansion",
    origin = "baseline",
    origin_start = 1
  )

# WARNING - overwriting an existing scenario
model_sim <- create_model(name = "SFC SIM demo", template = "SIM")

model_sim <- model_sim |>
  simulate_scenario(
    scenario = "baseline",
    max_iter = 350,
    periods = 100,
    hidden_tol = 0.1,
    tol = 1e-05,
    method = "Newton"
  )

model_sim <- model_sim |>
  simulate_scenario(
    scenario = "baseline",
    max_iter = 350,
    periods = 100,
    hidden_tol = 0.1,
    tol = 1e-05,
    method = "Newton"
  )


# OPEN --------------------------------------------------------------------

# create OPEN model from template
model_open <- create_model(name = "SFC OPEN demo", template = "OPEN")

# change default values
model_open <- model_open |>
  change_init(name = "alpha1_N", value = 0.5) |>
  change_init("alpha2_N", 0.5)

# simulate
model_open <- model_open |>
  simulate_scenario(
    scenario = "baseline",
    max_iter = 350,
    periods = 100,
    hidden_tol = 0.1,
    tol = 1e-05,
    method = "Gauss"
  )

plot_simulation(
  model = model_open, scenario = "baseline", from = 1, to = 100,
  expressions = c(
    "TB_N = X_N - IM_N",
    "TB_S = X_S - IM_S",
    "GB_N = TX_N - (G_N + dplyr::lag(r_N) * dplyr::lag(Bh_N))",
    "GB_S = TX_S - (G_S + dplyr::lag(r_S) * dplyr::lag(Bh_S))"
  )
)

# Scenario 1: Ever-falling gold reserves

shock <- create_shock() |>
  add_shock(
    variable = "mu_S",
    value = 0.25,
    desc = "What happens if there is an increase in the propensity to import in the South?",
    start = 5,
    end = 60
  )

model_open <- model_open |>
  add_scenario(
    name = "expansion", origin = "baseline", origin_period = 100,
    shock = shock
  )


model_open <- simulate_scenario(model_open,
  scenario = "expansion", max_iter = 350,
  periods = 60, hidden_tol = 0.1, tol = 1e-05, method = "Gauss"
)


plot_simulation(
  model = model_open, scenario = "expansion", from = 1, to = 60,
  expressions = c("Y_N", "Y_S")
)

# What happen with the trade and government balances?

plot_simulation(
  model = model_open, scenario = "expansion", from = 1, to = 60,
  expressions = c(
    "TB_N = X_N - IM_N",
    # " TB_S = X_S - IM_S",
    "GB_N = TX_N - (G_N + dplyr::lag(r_N) * dplyr::lag(Bh_N))",
    "GB_S = TX_S - (G_S + dplyr::lag(r_S) * dplyr::lag(Bh_S))",
    "deltaV_N = V_N - dplyr::lag(V_N)"
  )
)

plot_simulation(
  model = model_open, scenario = "expansion", from = 1, to = 60,
  expressions = c( # " TB_N = X_N - IM_N",
    "TB_S = X_S - IM_S",
    # " GB_N = TX_N - (G_N + dplyr::lag(r_N) * dplyr::lag(Bh_N))",
    "GB_S = TX_S - (G_S + dplyr::lag(r_S) * dplyr::lag(Bh_S))",
    # " deltaV_N = V_N - dplyr::lag(V_N)",
    "deltaV_S = V_S - dplyr::lag(V_S)"
  )
)

# what happens with the gold reserves of both countries?

plot_series(
  model = model_open, scenario = "expansion", from = 1, to = 60,
  expressions = c("or_S", "or_N")
)

# However, here it is nice to see what?s happening with the balance sheet of the
# Southern central bank:

plot_series(
  model = model_open, scenario = "expansion", from = 1, to = 60,
  expressions = c(
    "deltaBcb_S = Bcb_S - dplyr::lag(Bcb_S)",
    "deltaHs_S = Hs_S - dplyr::lag(Hs_S)"
  )
)

# sensitivity
model_sen <- create_sensitivity(model_open,
  variable = "alpha1_N",
  lower = 0,
  upper = 0.8,
  step = 0.1
)

model_sen <- model_sen |> simulate_scenario()

# plot results
plot_simulation(
  model = model_sen,
  scenario = "sensitivity",
  take_all = T,
  from = 1,
  to = 100,
  expressions = c("Y", "C_d")
)


# SIMEX --------------------------------------------------------------------

# SIMEX model added to templates
model_simex <- create_model(name = "SFC SIMEX demo", template = "SIMEX")

# change default values
model_simex <- model_simex |>
  change_init(name = "alpha1", value = 0.5)

# simulate
model_simex <- model_simex |>
  simulate_scenario(
    scenario = "baseline",
    max_iter = 350,
    periods = 100,
    hidden_tol = 0.1,
    tol = 1e-05,
    method = "Gauss"
  )

plot_simulation(
  model = model_simex, scenario = "baseline", from = 1, to = 100,
  expressions = c(
    "Y",
    "C_d"
  )
)
