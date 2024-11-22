{
  model <- create_model() |>
    add_variable("x", init = c(1:100)) |>
    add_variable("y", init = 50) |>
    add_variable("z") |>
    add_equation("y=x+4") |>
    add_equation("z=z[-4] + d(x[-4])") |>
    simulate_scenario(info = T, periods = 101)
  
  plot_simulation(model, expressions = c("x", "y", "z"))
  
  model_sens <- create_sensitivity(model, "x", lower = 1, upper = 10, step = 1) %>%
    simulate_scenario()
  
  plot_simulation(model_sens, scenario = "sensitivity", take_all = T, expressions = "z", from = 1, to = 20)  
}

{
model <- create_model() |>
  add_variable("x", init = c(1:100)) |>
  add_variable("y", init = 50) |>
  add_variable("z") |>
  add_equation("y=x+4") |>
  add_equation("z=z[-4] + d(x[-4])") |>
  simulate_scenario(info = T, periods = 101)

model$baseline$result
plot_simulation(model, expressions = c("x", "y", "z"))

shock <- create_shock() %>%
  add_shock("x", rate = 0.2, start = 25)

model <- model %>% add_scenario(origin_start = 50, shock = shock) %>%
  simulate_scenario()

model$expansion$result
plot_simulation(model, scenario = "expansion", expressions = c("x", "y", "z"))
}

{
model <- create_model(template = "SIM") %>%
  simulate_scenario()

shock <- create_shock() %>%
  add_shock("G_d", value = c(25,26), start = 5, end = 50)

model <- add_scenario(model, origin_start = 100, shock = shock) %>%
  simulate_scenario(periods = 100)

plot_simulation(model, scenario = "expansion", from = 1, to = 50, 
                expressions = c("Y", "G_s", "T_s"))
}

{
my_model <- create_model() |>
  add_variable("x", init = c(1,3,6,10)) |>
  add_variable("y", init = 1) |>
  add_variable("z") |>
  add_equation("y=x+4") |>
  add_equation("z=z[-4] + d(x[-4])") |>
  simulate_scenario(info = T, periods = 10)

my_model$baseline
}

{
  my_model <- create_model() |>
    add_variable("x", init = c(1,2,3,4)) |>
    add_variable("y", init = 1) |>
    add_variable("z") |>
    add_equation("y=exp(1)^(log(y[-1]) + d(log(x)) * d(log(z)))") |>
    add_equation("z=z[-1]*3") |>
    simulate_scenario(info = T)

  my_model$baseline
}

{
  my_model <- create_model() |>
    add_variable("par_x_1", 1) |>
    add_variable("x", 1) |>
    add_variable("P_x", 1) |>
    add_variable("par_x_2", 1) |>
    add_variable("P_m", 1) |>
    add_variable("par_x_3", 1) |>
    add_variable("m_W", 1) |>
    add_equation("x = exp(1)^(par_x_1*log(P_x[-1]) + par_x_2*log(P_m[-1]) + par_x_3*log(m_W))") |>
    simulate_scenario()

  my_model$baseline
}

{
  my_model <- create_model() |>
    add_variable("x",
      "y",
      init = 1,
      "z", init = 1
    ) |>
    add_equation("x = y + z") |>
    simulate_scenario()

  my_model$baseline
}

{
  my_model <- create_model() |>
    add_variable(
      "c", 0,
      "alfa", 0.5,
      "beta", 0.5,
      "y", 0,
      "epsilon", 0.5,
      "i", 0.5
    ) |>
    add_equation(
      "c = alfa + beta*y + epsilon",
      "y = c + i"
    ) |>
    simulate_scenario(periods = 20)

  my_model$baseline

  i_data = rnorm(20)
  
  my_shock <- create_shock() |>
    add_shock_vector("i", i_data, start = 1)

  my_model <- my_model |>
    add_scenario(shock = my_shock) |>
    simulate_scenario(periods = 20)

  my_model$expansion
}
