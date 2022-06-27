# godley (0.1.0) — an integrated approach to stock-flow consistent modelling

## Installation
To install godley package please use ```devtools```:
- set working directory to location with godley package files and use  ```install()``` function 
```
devtools::install(build_vignettes = TRUE)
library("godley")
```
or
- use ```install_github()``` function to install package directly from github:
```
devtools::install_github("gamrot/godley", build_vignettes = TRUE)
library("godley")
```

## Overview
godley is an R package for simulating SFC (stock-flow consistent) models. Package can be used to create, simulate and modify various model scenarios and sensitivities and to visualize simulation results. The vast majority of available components is presented in `vignettes` directory containing scripts with models from a book: *Monetary Economy (Godley & Lavoie, 2007)*.

## Functions
Main functions in package are: \
```create_model()``` — creates a SFC model, \
```add_variable()``` — adds variables to the SFC model, \
```add_equation()``` — adds equations to the SFC model, \
```change_init()``` — changes initial value for selected parameter, \
```prepare()``` — prepares model for simulation, \
```simulate_scenario()``` — calculates provided scenario(s) from the SFC model, \
```plot_simulation()``` — plots simulation results of provided scenario(s), \
```create_shock()``` — creates a SFC_shock object, \
```add_shock()``` — adds shock equations to the SFC_shock object, \
```add_scenario()``` — adds new scenario to the SFC model, \
```create_sensitivity()``` — creates a new SFC model with sensitivity scenarios for provided variable. 

## Example
Model SIM:
```
# create SIM model from available template
model_sim <- create_model(name = "SFC SIM example", template = "SIM")

# change initial value for alpha1
model_sim <- change_init(model = model_sim, name = "alpha1", value = 0.5)

# prepare scenario to simulate
model_sim <- prepare(model = model_sim) 

# simulate baseline scenario
model_sim <- simulate_scenario(model = model_sim, scenario = "baseline", max_iter = 350, 
			       periods = 100, hidden_tol = 0.1, tol = 1e-08, method = "Newton")

# add and simulate a shock - an increase in government expenditures
sim_shock <- create_shock() 

sim_shock <- add_shock(shock = sim_shock, equation = "G_d = 25", 
		       desc = "permanent increase in government expenditures", start = 5, end = 50)

model_sim <- add_scenario(model = model_sim, name = "expansion", origin = "baseline", 
			  origin_period = 100, shock = sim_shock)

model_sim <- simulate_scenario(model = model_sim, max_iter = 350, periods = 100, 
			       hidden_tol = 0.1, tol = 1e-08, method = "Gauss")

# plot results
plot_simulation(model = model_sim, scenario = c("baseline", "expansion"), take_all = FALSE, 
		from = 10, to = 60, expressions = c("Y", "C_d", "C_s / alpha1"))

# create sensitivity
model_sen <- create_sensitivity(model_pass = model_sim, variable = "alpha1", 
				lower = 0.1, upper = 0.7, step = 0.1)

# simulate sensitivity
model_sen <- simulate_scenario(model = model_sen, max_iter = 350, periods = 100, 
			       hidden_tol = 0.1, tol = 1e-08, method = "Gauss")

# plot sensitivity results
plot_simulation(model = model_sen, scenario = "sensitivity", take_all = TRUE, 
		from = 1, to = 100, expressions = c("Y", "C_d"))
```

## References
Package was created based on the book *Monetary Economics. An Integrated Approach to Credit, Money, Income, Production and Wealth.*  written by *Wynne Godley and Marc Lavoie, 2007*.
