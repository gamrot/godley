model_DIS <- create_model(name = 'DIS model') 


model_DIS <- model_DIS %>%
  #parameteres
  add_variable(., 'rl', init =  0.025) %>% 
  add_variable(., 'pr', init =  1) %>% 
  add_variable(., 'W', init =  0.75) %>% 
  add_variable(., 'add', init =  0.02) %>% 
  add_variable(., 'alpha0', init =  15) %>% 
  add_variable(., 'alpha1', init =  0.8) %>% 
  add_variable(., 'alpha2', init =  0.1) %>% 
  add_variable(., 'beta', init =  0.75) %>% 
  add_variable(., 'epsilon', init =  0.75) %>% 
  add_variable(., 'gamma', init =  0.25) %>% 
  add_variable(., 'phi', init =  0.25) %>% 
  add_variable(., 'sigma_T', init =  0.15) %>%
  add_variable(., 'y', init =  .001) %>%
  add_variable(., 'p', init =  .001) %>%
  add_variable(., 'NHUC', init =  .001) %>% 
  add_variable(., 's_E', init =  .001)

model_DIS <- model_DIS %>%
  #equations
  add_equation(., 'y = s_E + inv_E - inv[-1]') %>% 
  add_equation(., 'inv_T = sigma_T * s_E') %>% 
  add_equation(., 'inv_E = inv[-1] + gamma * (inv_T - inv[-1])') %>% 
  add_equation(., 'inv = inv[-1] + (y - s)') %>% 
  add_equation(., 's_E = beta * s[-1] + (1 - beta) * s_E[-1]') %>% 
  add_equation(., 's = c') %>% 
  add_equation(., 'N = y / pr') %>% 
  add_equation(., 'WB = N * W') %>% 
  add_equation(., 'UC = WB / y') %>% 
  add_equation(., 'INV = inv * UC') %>% 
  add_equation(., 'S = p * s') %>% 
  add_equation(., 'p = (1 + phi) * NHUC') %>% 
  add_equation(., 'NHUC = (1 - sigma_T) * UC + sigma_T * (1 + rl[-1]) * UC[-1]') %>% 
  add_equation(., 'EF = S - WB + (INV - INV[-1]) - rl[-1] * INV[-1]') %>% 
  add_equation(., 'Ld = INV') %>% 
  add_equation(., 'Ls = Ld') %>% 
  add_equation(., 'Ms = Ls') %>% 
  add_equation(., 'rm = rl - add') %>% 
  add_equation(., 'EFb = rl[-1] * Ls[-1] - rm[-1] * Mh[-1]') %>% 
  add_equation(., 'YD = WB + EF + EFb + rm[-1] * Mh[-1]') %>% 
  add_equation(., 'Mh = Mh[-1] + YD - C') %>% 
  add_equation(., 'ydhs = c + (mh - mh[-1])') %>% 
  add_equation(., 'C = c * p') %>% 
  add_equation(., 'mh = Mh / p') %>% 
  add_equation(., 'c = alpha0 + alpha1 * ydhs_E + alpha2 * mh[-1]') %>% 
  add_equation(., 'ydhs_E = epsilon * ydhs[-1] + (1 - epsilon) * ydhs_E[-1]') %>%
  add_equation(., 'Mh = Ms', hidden = T)
  
## validadion ----

model_DIS <- prepare(model_DIS)

## simulation ----

model_DIS <- simulate(model_DIS, scenario = 'baseline', max_iter = 350,
                       periods = 100, hidden_tol = 0.1, tol = 1e-08, method = 'Gauss')

plot_series(model = model_DIS, scenario = 'baseline', from = 1, to = 100,
            expressions = c('ydhs'))
                            
 
#Scenario 1: 

shock <- create_shock() %>% 
  add_shock(equation = 'phi = .35',
            desc = 'One-shot increase in the costing margin',
            start = 5,
            end = 40) 

model_DIS <- model_DIS %>% 
  add_scenario(name = 'expansion', origin = 'baseline',origin_period = 100, 
               shocks = shock)

model_DIS <- simulate(model_DIS, scenario = 'expansion', max_iter = 350,
                       periods = 40, hidden_tol = .1, tol = 1e-08, method = 'Gauss')

plot_series(model = model_DIS, scenario = 'expansion', from = 1, to = 40,
            expressions = c("c", "ydhs"))

#Scenario 2:  

shock2 <- create_shock() %>% 
  add_shock(equation = 'sigma_T  = .25',
            desc = 'ncrease in the target inventories to sales ratio',
            start = 5,
            end = 50) 

model_DIS <- model_DIS %>% 
  add_scenario(name = 'expansion2', origin = 'baseline',origin_period = 100, 
               shocks = shock2)

model_DIS <- simulate(model_DIS, scenario = 'expansion2', max_iter = 350,
                       periods = 50, hidden_tol = .1, tol = 1e-08, method = 'Gauss')

plot_series(model = model_DIS, scenario = 'expansion2', from = 1, to = 60,
            expressions = c("ydhs", "c"))

plot_series(model = model_DIS, scenario = 'expansion2', from = 1, to = 60,
            expressions = c("delta_inv = inv - lag(inv)",
                            "delta_inv_E = inv_E - lag(inv_E)"))






