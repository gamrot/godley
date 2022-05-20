

model_BMW <- create_model(name = 'SFC BMW')  %>%
    #parameters  
  add_variable(., 'rl', init = 0.025) %>% 
  add_variable(., 'alpha0', init = 20) %>% 
  add_variable(., 'alpha1', init = 0.75) %>% 
  add_variable(., 'alpha2', init = 0.10) %>% 
  add_variable(., 'delta', init =  0.10) %>% 
  add_variable(., 'gamma', init = 0.15) %>% 
  add_variable(., 'kappa', init =  1) %>% 
  add_variable(., 'pr', init =  1)  %>%
  add_variable(., 'Nd', init = .001) %>%
  add_variable(., 'Ns', init = .001)  %>% 
  add_variable(., 'Y', init = .001) 
  
  # add_variable(., 'Ls', init =  12) %>% 
  # add_variable(., 'Ld', init = 12) %>% 
  # add_variable(., 'Mh', init = 12) %>% 
  # add_variable(., 'Ms', init = 12) %>% 
  # add_variable(., 'Nd', init = 128) 
  # 
  #equations
model_BMW <- model_BMW %>%  
  add_equation(., 'Cs = Cd') %>%
  add_equation(., 'Is = Id') %>%
  add_equation(., 'Ns = Nd') %>%
  add_equation(., 'Ls = Ls[-1] + Ld - Ld[-1]') %>% 
  add_equation(., 'Y = Cs + Is') %>% 
  add_equation(., 'WBd = Y - rl[-1] * Ld[-1] - AF') %>% 
  add_equation(., 'AF = delta * K[-1]') %>% 
  add_equation(., 'Ld = Ld[-1] + Id - AF') %>% 
  add_equation(., 'YD = WBs + rm[-1] * Mh[-1]') %>%
  add_equation(., 'Mh = Mh[-1] + YD - Cd') %>%
  add_equation(., 'Ms = Ms[-1] + Ls - Ls[-1]') %>%
  add_equation(., 'rm = rl') %>%
  add_equation(., 'WBs = W * Ns') %>%
  add_equation(., 'Nd = Y / pr') %>%
  add_equation(., 'W = WBd / Nd') %>%
  add_equation(., 'Cd = alpha0 + alpha1 * YD + alpha2 * Mh[-1]') %>%
  add_equation(., 'K = K[-1] + Id - DA') %>%
  add_equation(., 'DA = delta * K[-1]') %>%
  add_equation(., 'KT = kappa * Y[-1]') %>%
  add_equation(., 'Id = gamma * (KT - K[-1]) + DA') %>%
  add_equation(., 'Ms = Mh', hidden = T) 

## validadion ----

model_BMW <- prepare(model_BMW)

## simulation ----

model_BMW <- simulate(model_BMW, scenario = 'baseline', max_iter = 350,
                        periods = 100, hidden_tol = 0.1, tol = 1e-08, method = 'Gauss')


# let’s check that the model arrived to a steady state income level

plot_series(model = model_BMW, scenario = 'baseline', from = 1, to = 100,
            expressions = c('Y'))


# Scenario 1: 

shock <- create_shock() %>% 
  add_shock(equation = 'alpha0 = 30',
            desc = 'An increase in autonomous consumption expenditures',
            start = 5,
            end = 60) 

model_BMW <- model_BMW %>% 
  add_scenario(name = 'expansion', origin = 'baseline',origin_period = 100, 
               shocks = shock)

model_BMW <- simulate(model_BMW, scenario = 'expansion', max_iter = 350,
                       periods = 60, hidden_tol = 10, tol = 1e-08, method = 'Newton')

plot_series(model = model_BMW, scenario = 'expansion', from = 1, to = 60,
            expressions = c("Cd", "YD"))

plot_series(model = model_BMW, scenario = 'expansion', from = 1, to = 60,
            expressions = c("Id", "AF"))


# Scenario 2: 


shock2 <- create_shock() %>% 
  add_shock(equation = 'alpha1 = .7',
            desc = 'Increase in the propensity to save',
            start = 5,
            end = 60) 

model_BMW <- model_BMW %>% 
  add_scenario(name = 'expansion2', origin = 'baseline',origin_period = 100, 
               shocks = shock2)

model_BMW <- simulate(model_BMW, scenario = 'expansion2', max_iter = 350,
                      periods = 60, hidden_tol = .1, tol = 1e-08, method = 'Gauss')

plot_series(model = model_BMW, scenario = 'expansion2', from = 1, to = 60,
            expressions = c("Cd", "YD"))s

plot_series(model = model_BMW, scenario = 'expansion2', from = 1, to = 60,
            expressions = c('YK = Y / lag(K)'))


