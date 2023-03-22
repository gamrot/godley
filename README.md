# godley

`godley` is an R package for simulating SFC (stock-flow consistent) models. It can be used to create and simulate fully fledged post-keynesian / MMT models of the economy. It allows users to apply shocks, analyse effects of parameter change, visualize different macro scenarios and much more.

`godley` is named after Wynne Godley (1926-2010), a famous British post-keynesian economist and a father of stock-flow consistent modelling.

## Installation ⚙️

The best way to start using `godley` is to install it directly from GitHub using the `devtools` package.

``` r
install.packages("devtools")
devtools::install_github("gamrot/godley")
```

## Usage 📊

Below you can find a simple example of `godley` in action. Let's play with the well known "SIM model" from Monetary Economics (Godley & Lavoie, 2007).

First, we need to create an empty model using `create_model()` function.

``` r
model_sim <- create_model(name = "SFC SIM")
```

Now let's add some variables using the `add_variable()` function. This will add a `$varibles` tibble to the model. Each variable can be assigned with an initial value --- this can be both a theoretical starting point or a whole vector of real data.

``` r
model_sim <- model_sim |>
  add_variable(
    "C_d", desc = "Consumption demand by households",
    "C_s", desc = "Consumption supply",
    "G_s", desc = "Government supply",
    "H_h", desc = "Cash money held by households",
    "H_s", desc = "Cash money supplied by the government",
    "N_d", desc = "Demand for labor",
    "N_s", desc = "Supply of labor",
    "T_d", desc = "Taxes, demand",
    "T_s", desc = "Taxes, supply",
    "Y", desc = "Income = GDP",
    "Yd", desc = "Disposable income of households",
    "alpha1", init = 0.6, desc = "Propensity to consume out of income",
    "alpha2", init = 0.4, desc = "Propensity to consume out of wealth",
    "theta", init = 0.2, desc = "Tax rate",
    "G_d", init = 20, desc = "Government demand",
    "W", init = 1, desc = "Wage rate"
  )

model_sim$variables

## # A tibble: 16 × 3
##    name   init      desc
##    <chr>  <list>    <chr>
##  1 C_d    0         Consumption demand by households
##  2 C_s    0         Consumption supply
##  3 G_s    0         Government supply
##  4 H_h    0         Cash money held by households
##  5 H_s    0         Cash money supplied by the government
##  6 N_d    0         Demand for labor
##  7 N_s    0         Supply of labor
##  8 T_d    0         Taxes, demand
##  9 T_s    0         Taxes, supply
## 10 Y      0         Income = GDP
## 11 Yd     0         Disposable income of households
## 12 alpha1 0.6       Propensity to consume out of income
## 13 alpha2 0.4       Propensity to consume out of wealth
## 14 theta  0.2       Tax rate
## 15 G_d    20        Government demand
## 16 W      1         Wage rate
```

Okay, let's add some equations, shall we? There's a function for that! You've guessed it, it's the `add_equation()` function. It also adds a tibble to the model, this time it's called `$equations`.

Lags in equation formulas can be denoted by `[-1]`. First order differences have their own function `d()`. To achieve a lagged difference you just need to combine these two, e.g., `d(H_s[-1])`.

``` r
model_sim <- model_sim |>
  add_equation(
    "C_s = C_d", desc = "Consumption",
    "G_s = G_d",
    "T_s = T_d",
    "N_s = N_d",
    "Yd = W * N_s - T_s",
    "T_d = theta * W * N_s",
    "C_d = alpha1 * Yd + alpha2 * H_h[-1]",
    "H_s = G_d - T_d + H_s[-1]",
    "H_h = Yd - C_d + H_h[-1]",
    "Y = C_s + G_s",
    "N_d = Y/W",
    "H_s = H_h", desc = "Money equilibrium", hidden = TRUE
  )

model_sim$equations

## # A tibble: 12 × 3
##    equation                             hidden desc               
##    <chr>                                <lgl>  <chr>              
##  1 C_s = C_d                            FALSE  "Consumption"      
##  2 G_s = G_d                            FALSE  ""                 
##  3 T_s = T_d                            FALSE  ""                 
##  4 N_s = N_d                            FALSE  ""                 
##  5 Yd = W * N_s - T_s                   FALSE  ""                 
##  6 T_d = theta * W * N_s                FALSE  ""                 
##  7 C_d = alpha1 * Yd + alpha2 * H_h[-1] FALSE  ""                 
##  8 H_s = G_d - T_d + H_s[-1]            FALSE  ""                 
##  9 H_h = Yd - C_d + H_h[-1]             FALSE  ""                 
## 10 Y = C_s + G_s                        FALSE  ""                 
## 11 N_d = Y/W                            FALSE  ""                 
## 12 H_s = H_h                            TRUE   "Money equilibrium"
```

With all variables and equations defined, it's about time to run some simulations using the `simulate_scenario()` function. You can choose a number of periods, a simulation method (`Newton` or `Gauss`) and optionally a start date for quarters. Results will be stored in a `$result` tibble under a `$baseline` scenario.

``` r
model_sim <- model_sim |>
  simulate_scenario(
    periods = 100, start_date = "2015-01-01",
    method = "Gauss", max_iter = 350, tol = 1e-05
  )

model_sim$baseline$result

## # A tibble: 100 × 17
##    time         C_s   G_s   T_s   N_s    Yd   T_d   C_d   H_s   H_h     Y   N_d
##    <date>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 2015-01-01   0       0  0      0     0    0      0     0     0     0     0  
##  2 2015-04-01  18.5    20  7.69  38.5  30.8  7.69  18.5  12.3  12.3  38.5  38.5
##  3 2015-07-01  27.9    20  9.59  47.9  38.3  9.59  27.9  22.7  22.7  47.9  47.9
##  4 2015-10-01  35.9    20 11.2   55.9  44.8 11.2   35.9  31.5  31.5  55.9  55.9
##  5 2016-01-01  42.7    20 12.5   62.7  50.2 12.5   42.7  39.0  39.0  62.7  62.7
##  6 2016-04-01  48.5    20 13.7   68.5  54.8 13.7   48.5  45.3  45.3  68.5  68.5
##  7 2016-07-01  53.3    20 14.7   73.3  58.6 14.7   53.3  50.6  50.6  73.3  73.3
##  8 2016-10-01  57.4    20 15.5   77.4  61.9 15.5   57.4  55.2  55.2  77.4  77.4
##  9 2017-01-01  60.9    20 16.2   80.9  64.7 16.2   60.9  59.0  59.0  80.9  80.9
## 10 2017-04-01  63.8    20 16.8   83.8  67.1 16.8   63.8  62.2  62.2  83.8  83.8
## # … with 90 more rows, and 5 more variables: alpha1 <dbl>, alpha2 <dbl>,
## #   theta <dbl>, G_d <dbl>, W <dbl>
```

When everything is done, you can plot the outcome using the `plot_simulation()` function. You can define which variables or expressions you want. Let's plot Income, Government spending and Taxes.

``` r
plot_simulation(
  model_sim, scenario = "baseline",
  from = "2015-01-01", to = "2023-01-01",
  expressions = c("Y", "G_s", "T_s")
)
```

![Scenario baseline](man/figures/images/Scenario_baseline.png)

And one more thing (if you're lazy like me), you can create models with "templates" using `create_model(template = "SIM")`. You can choose from `SIM`, `SIMEX`, `PC`, `PCEX`, `LP`, `REG`, `OPEN`, `BMW`, `BMWK`, `DIS` and `DISINF`. Basically all models from Godley & Lavoie (2007).

### Shocks ⚡

`godley` allows you to create and simulate shocks. Let's see what happens if we increase government spending.

To create the shock first we need to create an empty shock object with `create_shock()`. Next let's see what's gonna happen when we use the `add_shock()` function to increase government spending by 20% from Q1 2017 to Q4 2020. Shock value can be defined explicitly by `value`, relatively by `rate` or absolutely by `absolute`.

``` r
sim_shock <- create_shock() |>
  add_shock(
    variable = "G_d", rate = 0.2,
    start = "2017-01-01", end = "2020-10-01",
    desc = "permanent increase in government expenditures"
  )
```

With everything defined, let's add a new scenario using the `add_scenario()` function. But first we need to instruct `godley` which scenario we will use as a starting point (and which periods). After a shock scenario is created we can simulate it using the now familiar `simulate_scenario()` function.

``` r
model_sim <- model_sim |>
  add_scenario(
    name = "expansion", origin = "baseline",
    origin_start = "2015-01-01", origin_end = "2023-10-01",
    shock = sim_shock
  ) |>
  simulate_scenario(periods = 100)

model_sim$expansion$result

## # A tibble: 100 × 17
##    time         C_s   G_s   T_s   N_s    Yd   T_d   C_d   H_s   H_h     Y   N_d
##    <date>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 2015-01-01   0       0  0      0     0    0      0     0     0     0     0  
##  2 2015-04-01  18.5    20  7.69  38.5  30.8  7.69  18.5  12.3  12.3  38.5  38.5
##  3 2015-07-01  27.9    20  9.59  47.9  38.3  9.59  27.9  22.7  22.7  47.9  47.9
##  4 2015-10-01  35.9    20 11.2   55.9  44.8 11.2   35.9  31.5  31.5  55.9  55.9
##  5 2016-01-01  42.7    20 12.5   62.7  50.2 12.5   42.7  39.0  39.0  62.7  62.7
##  6 2016-04-01  48.5    20 13.7   68.5  54.8 13.7   48.5  45.3  45.3  68.5  68.5
##  7 2016-07-01  53.3    20 14.7   73.3  58.6 14.7   53.3  50.6  50.6  73.3  73.3
##  8 2016-10-01  57.4    20 15.5   77.4  61.9 15.5   57.4  55.2  55.2  77.4  77.4
##  9 2017-01-01  64.6    24 17.7   88.6  70.9 17.7   64.6  61.4  61.4  88.6  88.6
## 10 2017-04-01  69.4    24 18.7   93.4  74.7 18.7   69.4  66.8  66.8  93.4  93.4
## # … with 90 more rows, and 5 more variables: alpha1 <dbl>, alpha2 <dbl>,
## #   theta <dbl>, G_d <dbl>, W <dbl>
```

Now let's see the result using the `plot_simulation()` function. As you can see, an increase in government expenditures has a positive effect on income... And not so positive short-term effect on government balance.

``` r
plot_simulation(
  model_sim, scenario = "expansion",
  to = "2025-01-01", expressions = c("Y", "G_s", "T_s")
)
```

![Scenario expansion](man/figures/images/Scenario_expansion.png)

### Sensitivity 🧙

`godley` allows users to see if simulation results are sensitive to parameter changes. After all, we don't want to create models that make sense just for a specific combination of parameters. Let's see how small changes to `alpha1` affect short-term model dynamics.

First we need to create a new model object using `create_sensitivity()` function and define lower and upper bounds for the parameter we want to analyze. After the model is created we can simulate all scenarios.

``` r
model_sens <- model_sim |>
  create_sensitivity(
    variable = "alpha1", lower = 0.1, upper = 0.8, step = 0.1
  ) |>
  simulate_scenario(periods = 100, start_date = "2015-01-01")
```

Now we're ready to see results on a plot. All sensitivity scenarios can be plotted at once by taking all scenarios containing selected name.

``` r
plot_simulation(
  model_sens, scenario = "sensitivity", take_all = TRUE,
  to = "2028-01-01", expressions = c("Y")
)
```

![Sensitivity](man/figures/images/Sensitivity.png)

### Other examples ⭐

[Here](https://gamrot.github.io/godley/) you can find more details about package functions and other models created with `godley`.

## Functions 🔧

Here's a list of package's most important functions.

`create_model()` - creates an SFC model\
`add_variable()` - adds variables\
`add_equation()` - adds equations\
`simulate_scenario()` - simulates selected scenario(s)\
`plot_simulation()` - plots simulation results

`create_shock()` - creates a shock object\
`add_shock()` - adds shock parameters\
`add_scenario()` - adds scenario to an existing model

`create_sensitivity()` - creates a new SFC model with sensitivity scenarios for selected parameters

## Similar work 👪

There are two other packages that also allows users to build stock-flow consistent models:

-   [sfcr](https://github.com/joaomacalos/sfcr) - you should definitely check it out!
-   [pysolve3](https://github.com/gpetrini/pysolve3)

## Getting help 🐛

If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/gamrot/godley/issues).
