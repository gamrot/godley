# godley (0.1.0) - an integrated approach to stock-flow consistent modelling
Package godley is an R package for simulating SFC (stock-flow consistent) models. Package can be used to create, simulate and modify various model scenarios, calculate sensitivities and visualize simulations results. 

## Installation
There are two ways to install godley package with ```devtools```:

- use ```install_github()``` function to install package directly from github:
```
devtools::install_github("gamrot/godley", build_vignettes = TRUE)
library("godley")
```
- or set working directory to location with godley package files and use ```install()``` function:
```
devtools::install(build_vignettes = TRUE)
library("godley")
```
## Example
Below you can find an example of application godley package to simulate model SIM from Chapter 3 *Monetary Economics: (Godley & Lavoie, 2007)*. More examples from the book will be described in the *articles*.

### Baseline scenario
To create model SIM you need to create first an empty SFC model using ```create_model()``` function:
```
# Create empty model
model_sim <- create_model(name = "SFC SIM")
```

#### Variables
When new empty SFC model is created you can add all variables that will be used in simulation using ```add_variable()``` function. As a result the tibble with defined variables will be created in the model:
```
# Add variables
model_sim <- model_sim %>%
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

model_sim$variables

## # A tibble: 16 x 3
##    name   desc                                   init
##    <chr>  <chr>                                 <dbl>
##  1 C_d    Consumption demand by households        0  
##  2 C_s    Consumption supply                      0  
##  3 G_s    Government supply                       0  
##  4 H_h    Cash money held by households           0  
##  5 H_s    Cash money supplied by the government   0  
##  6 N_d    Demand for labor                        0  
##  7 N_s    Supply of labor                         0  
##  8 T_d    Taxes, demand                           0  
##  9 T_s    Taxes, supply                           0  
## 10 Y      Income = GDP                            0  
## 11 Yd     Disposable income of households         0  
## 12 alpha1 Propensity to consume out of income     0.6
## 13 alpha2 Propensity to consume out of wealth     0.4
## 14 theta  Tax rate                                0.2
## 15 G_d    Government demand                      20  
## 16 W      Wage rate                               1
```
#### Equations
As a next step you need to add all equations that will be used in simulation using ```add_equation()``` function. As a result the tibble with defined equations will be created in the model:
```
# Add equations
model_sim <- model_sim %>%
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
  
model_sim$equation

## # A tibble: 12 x 3
##    equation                             desc                hidden
##    <chr>                                <chr>               <lgl> 
##  1 C_s = C_d                            "Consumption"       FALSE 
##  2 G_s = G_d                            ""                  FALSE 
##  3 T_s = T_d                            ""                  FALSE 
##  4 N_s = N_d                            ""                  FALSE 
##  5 Yd = W * N_s - T_s                   ""                  FALSE 
##  6 T_d = theta * W * N_s                ""                  FALSE 
##  7 C_d = alpha1 * Yd + alpha2 * H_h[-1] ""                  FALSE 
##  8 H_s = G_d - T_d + H_s[-1]            ""                  FALSE 
##  9 H_h = Yd - C_d + H_h[-1]             ""                  FALSE 
## 10 Y = C_s + G_s                        ""                  FALSE 
## 11 N_d = Y/W                            ""                  FALSE 
## 12 H_s = H_h                            "Money equilibrium" TRUE
```
#### Simulation

If all variables and equations are defined, you can simulate created scenario with ```simulate_scenario()``` function. You should define i.a. method of simulation (Newton or Gauss) and number of periods. In this example simulation will be performed for next 100 periods using Newton method. Results of simulation are stored in the *result* tibble:

```
# simulate baseline scenario
model_sim <- simulate_scenario(model = model_sim, scenario = "baseline", max_iter = 350, 
                               periods = 100, hidden_tol = 0.1, tol = 1e-08, method = "Newton")

model_sim$baseline$result

## # A tibble: 100 x 16
##      C_s   G_s   T_s   N_s    Yd   T_d   C_d   H_s   H_h     Y   N_d alpha1
##    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
##  1   0       0  0      0     0    0      0     0     0     0     0      0.5
##  2  13.3    20  6.67  33.3  26.7  6.67  13.3  13.3  13.3  33.3  33.3    0.5
##  3  22.2    20  8.44  42.2  33.8  8.44  22.2  24.9  24.9  42.2  42.2    0.5
##  4  29.9    20  9.99  49.9  39.9  9.99  29.9  34.9  34.9  49.9  49.9    0.5
##  5  36.6    20 11.3   56.6  45.3 11.3   36.6  43.6  43.6  56.6  56.6    0.5
##  6  42.4    20 12.5   62.4  49.9 12.5   42.4  51.1  51.1  62.4  62.4    0.5
##  7  47.4    20 13.5   67.4  53.9 13.5   47.4  57.6  57.6  67.4  67.4    0.5
##  8  51.7    20 14.3   71.7  57.4 14.3   51.7  63.3  63.3  71.7  71.7    0.5
##  9  55.5    20 15.1   75.5  60.4 15.1   55.5  68.2  68.2  75.5  75.5    0.5
## 10  58.8    20 15.8   78.8  63.0 15.8   58.8  72.4  72.4  78.8  78.8    0.5
## # ... with 90 more rows, and 4 more variables: alpha2 <dbl>, theta <dbl>,
## #   G_d <dbl>, W <dbl>
```

#### Plot
When simulation is done you can plot results using ```plot_simulation()``` function. In this function you should define which set of variables and/or expressions will be displayed on the plot. In this example Income, Government spending and Taxes will be shown:
```
# Plot scenario results
plot_simulation(model = model_sim, scenario = c("baseline"), from = 1, to = 50, 
                expressions = c("Y", "G_s", "T_s"))

```
![Scenario baseline](images_sim/Scenario_baseline.png)

As you can see on the plot above in steady state the Income is equal 100, while the Government spending and Taxes are both equals 20.

### Shock scenario
Package godley allows to create and simulate shock scenarios. In this example shock with an increased government expenditures to 25 will be implemented.

#### Shock equation
 To create the shock first you need to create empty shock object with ```create_shock()``` function and add new shock equation with ```add_shock()``` function. In this function you should define starting and ending periods for the applied shock:
```
# create an empty shock
sim_shock <- create_shock() 

# add a shock equation - an increase in government expenditures to 25
sim_shock <- add_shock(shock = sim_shock, equation = "G_d = 25", 
		       desc = "permanent increase in government expenditures", start = 5, end = 50)

sim_shock

## # A tibble: 1 x 4
##   equation desc                                          start   end
##   <chr>    <chr>                                         <dbl> <dbl>
## 1 G_d = 25 permanent increase in government expenditures     5    50
```
#### Shock scenario
When equation is defined, as a next step you need to add this shock as a new scenario into created earlier SFC model using ```add_scenario()``` function. In this function you need to define which period of original scenario will be treated as first period of the shock scenario: 
```
# create new scenario with shock equation
model_sim <- add_scenario(model = model_sim, name = "expansion", origin = "baseline", 
			  origin_period = 100, shock = sim_shock)

```
#### Shock simulation
After scenario is created you should simulate it:
```
# simulate shock scenario
model_sim <- simulate_scenario(model = model_sim, max_iter = 350, periods = 100, 
			       hidden_tol = 0.1, tol = 1e-08, method = "Newton")

model_sim$expansion$result

## # A tibble: 100 x 16
##      C_s   G_s   T_s   N_s    Yd   T_d   C_d   H_s   H_h     Y   N_d alpha1
##    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
##  1   0       0  0      0     0    0      0     0     0     0     0      0.5
##  2  13.3    20  6.67  33.3  26.7  6.67  13.3  13.3  13.3  33.3  33.3    0.5
##  3  22.2    20  8.44  42.2  33.8  8.44  22.2  24.9  24.9  42.2  42.2    0.5
##  4  29.9    20  9.99  49.9  39.9  9.99  29.9  34.9  34.9  49.9  49.9    0.5
##  5  39.9    25 13.0   64.9  51.9 13.0   39.9  46.9  46.9  64.9  64.9    0.5
##  6  47.9    25 14.6   72.9  58.4 14.6   47.9  57.3  57.3  72.9  72.9    0.5
##  7  54.9    25 16.0   79.9  63.9 16.0   54.9  66.4  66.4  79.9  79.9    0.5
##  8  60.9    25 17.2   85.9  68.7 17.2   60.9  74.2  74.2  85.9  85.9    0.5
##  9  66.1    25 18.2   91.1  72.9 18.2   66.1  80.9  80.9  91.1  91.1    0.5
## 10  70.6    25 19.1   95.6  76.5 19.1   70.6  86.8  86.8  95.6  95.6    0.5
## # ... with 90 more rows, and 4 more variables: alpha2 <dbl>, theta <dbl>,
## #   G_d <dbl>, W <dbl>
```
#### Plot
Now you can display results of calculated shock scenario. As you can see below due to increase of government expenditures to 25 the Income in steady state has increased to 125 and Taxes increased to level of government expenditures (25).

```
# plot results
plot_simulation(model = model_sim, scenario = c("expansion"), from = 1, to = 50, 
                expressions = c("Y", "G_s", "T_s"))
```
![Scenario extansion](images_sim/Scenario_expansion.png)

### Sensitivity
Package godley allows user to create and calculate various sensitivities. In this example you will find guidance how to calculate sensitivity for variable *alpha1* from model SIM.
#### Sensitivity model
First you need to create sensitivity model using ```create_sensitivity()``` function and define lower and upper boundaries for the variable and the step of sensitivity. As a results new SFC model with defined sensitivity scenarios will be created:
```
# create sensitivity
model_sens <- create_sensitivity(model_pass = model_sim, variable = "alpha1", 
				 lower = 0.1, upper = 0.7, step = 0.05)
```
#### Sensitivity simulation
Next you should simulate all sensitivity scenarios:
```
# simulate sensitivity
model_sens <- simulate_scenario(model = model_sens, max_iter = 350, periods = 100, 
                                hidden_tol = 0.1, tol = 1e-08, method = "Newton")
```

#### Plot
After simulation is completed you can plot and compare sensitivity results:
```
# plot sensitivity results
plot_simulation(model = model_sens, scenario = "sensitivity", take_all = TRUE, 
		            from = 1, to = 50, expressions = c("Y"))
```
![Sensitivity](images_sim/Sensitivity.png)


## Functions
Main functions in package godley described above in *Example* section are: \
```create_model()``` - creates a SFC model, \
```add_variable()``` - adds variables to the SFC model, \
```add_equation()``` - adds equations to the SFC model, \
```simulate_scenario()``` - calculates provided scenario(s) from the SFC model, \
```plot_simulation()``` - plots simulation results of provided scenario(s), \
```create_shock()``` - creates a SFC_shock object, \
```add_shock()``` - adds shock equations to the SFC_shock object, \
```add_scenario()``` - adds new scenario to the SFC model, \
```create_sensitivity()``` - creates a new SFC model with sensitivity scenarios for provided variable. 

## Similar packages
There are two other packages that also allows users to build stock-flow consistent models: 

- R package sfcr (https://github.com/joaomacalos/sfcr) 
- Python package pysolve3 (https://github.com/gpetrini/pysolve3)

## References
Package godley was created based on the book *Monetary Economics. An Integrated Approach to Credit, Money, Income, Production and Wealth.*  written by *Wynne Godley and Marc Lavoie, 2007*.
