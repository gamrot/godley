
model_pc <- create_model(name = 'SFC PC testowy') %>% 
  # variables
  add_variable(., 'B_cb', desc = '') %>%
  add_variable(., 'H_s', desc = '') %>%
  add_variable(., 'B_s', desc = '') %>%
  add_variable(., 'B_h', desc = '') %>%
  add_variable(., 'H_h1', desc = '') %>%
  add_variable(., 'H_h', desc = '') %>%
  add_variable(., 'C', desc = '') %>%
  add_variable(., 'V', desc = '') %>%
  add_variable(., 'T_x', desc = '') %>%
  add_variable(., 'Y', desc = 'Income = GDP') %>%
  add_variable(., 'Yd', desc = 'Disposable income of households') 



# parameters

model_pc <- model_pc %>%
  add_variable(., 'alpha1', init = 0.6,
               desc = 'Propensity to consume out of income') %>%
  add_variable(., 'alpha2', init = 0.4,
               desc = 'Propensity to consume out of wealth') %>%
  add_variable(., 'theta', init = 0.2, desc = 'Tax rate') %>%
  add_variable(., 'r', init = 0.025, desc = '') %>%
  add_variable(., 'G', init = 20, desc = 'Government demand') %>%
  add_variable(., 'lambda0', init = 0.635, desc = '')  %>%
  add_variable(., 'lambda1', init = 0.05, desc = '')  %>%
  add_variable(., 'lambda2', init = 0.01, desc = '')   



## equations

model_pc <- model_pc %>%
  add_equation(., 'Y = C + G', desc = '') %>%
  add_equation(., 'Yd = Y - T_x + r[-1] * B_h[-1]') %>%
  add_equation(., 'T_x = theta * (Y + r[-1] * B_h[-1])') %>%
  add_equation(., 'V = V[-1] + (Yd - C)') %>%
  add_equation(., 'C = alpha1 * Yd + alpha2 * V[-1]') %>%
  add_equation(., 'H_h = V - B_h') %>%
  add_equation(., 'H_h1 = V * ((1 - lambda0) - lambda1 * r + lambda2 * ( Yd/V ))') %>%
  add_equation(., 'B_h = V * (lambda0 + lambda1 * r - lambda2 * ( Yd/V ))') %>% 
  add_equation(., 'B_s = B_s[-1] + (G + r[-1] * B_s[-1]) - (T_x + r[-1] * B_cb[-1])') %>%
  add_equation(., 'H_s = H_s[-1] + B_cb - B_cb[-1]') %>%
  add_equation(., 'B_cb = B_s - B_h') %>%
  add_equation(., 'H_h = H_s', hidden = T)
## validadion ----

model_pc <- prepare(model_pc)

## simulation ----

model_pc <- simulate(model_pc, scenario = 'baseline', max_iter = 350,
                  periods = 100, hidden_tol = 0.1, tol = 1e-08, method = 'Gauss')

# Scenario 1: Increase in the rate of interest on bills 

shock <- create_shock() %>% 
  add_shock(equation = 'r = 0.035',
            desc = 'Increase in the rate of interest on bills',
            start = 5,
            end = 60) 

model_pc <- model_pc %>% 
  add_scenario(name = 'expansion', origin = 'baseline',origin_period = 100, 
               shocks = shock)

model_pc <- simulate(model_pc, scenario = 'expansion', max_iter = 350,
                      periods = 100, hidden_tol = 0.1, tol = 1e-08, method = 'Gauss')



plot_series(model = model_pc, scenario = 'expansion', from = 1, to = 50,
            expressions = c(' B_h / V'))
plot_series(model = model_pc, scenario = 'expansion', from = 1, to = 50,
            expressions = c(' H_h / V'))

