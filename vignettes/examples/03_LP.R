# model LP

# Create empty model
model_lp <- create_model(name = "SFC LP")

# Add variables
model_lp <- model_lp %>%
  add_variable("Y", desc = "") %>%
  add_variable("YDr") %>%
  add_variable("TX") %>%
  add_variable("V", init = 0.0000001) %>%
  add_variable("CG") %>%
  add_variable("C") %>%
  add_variable("VE", init = 0.0000001) %>%
  add_variable("Hh") %>%
  add_variable("Hd") %>%
  add_variable("Bd") %>%
  add_variable("BLd") %>%
  add_variable("BLh", desc = "") %>%
  add_variable("Bs", desc = "") %>%
  add_variable("Hs", desc = "") %>%
  add_variable("Bcb", desc = "") %>%
  add_variable("BLs", desc = "") %>%
  add_variable("ERrbl", desc = "") %>%
  add_variable("rbl", desc = "", ) %>%
  add_variable("CGE", desc = "") %>%
  add_variable("YDEr", desc = "") %>%
  add_variable("pebl", desc = "") %>%
  add_variable("Bh", desc = "") %>%
  add_variable("alpha1", init = 0.8, desc = "Propensity to consume out of income") %>%
  add_variable("alpha2", init = 0.2, desc = "Propensity to consume out of wealth") %>%
  add_variable("theta", init = 0.1938, desc = "Tax rate") %>%
  add_variable("rb", init = 0.03, desc = "") %>%
  add_variable("G", init = 20, desc = "Government demand") %>%
  add_variable("pbl", init = 20, desc = "") %>%
  add_variable("lambda20", init = 0.44196, desc = "") %>%
  add_variable("lambda22", init = 1.1, desc = "") %>%
  add_variable("lambda23", init = -1, desc = "") %>%
  add_variable("lambda24", init = -0.03, desc = "") %>%
  add_variable("lambda30", init = .3997, desc = "") %>%
  add_variable("lambda32", init = -1, desc = "") %>%
  add_variable("lambda33", init = 1.1, desc = "") %>%
  add_variable("lambda34", init = -0.03, desc = "") %>%
  add_variable("chi", init = 0.1, desc = "")

# Add equations
model_lp <- model_lp %>%
  add_equation("Y=C + G", desc = "") %>%
  add_equation("YDr= Y - TX + rb[-1] * Bh[-1] + BLh[-1]") %>%
  add_equation("TX =theta * (Y + rb[-1] * Bh[-1] + BLh[-1])") %>%
  add_equation("V = V[-1] + (YDr - C) + CG") %>%
  add_equation("CG = (pbl - pbl[-1]) * BLh[-1]") %>%
  add_equation("C = alpha1 * YDEr + alpha2 * V[-1]") %>%
  add_equation("VE = V[-1] + (YDEr - C) + CG") %>%
  add_equation("Hh = V - Bh - pbl * BLh") %>%
  add_equation("Hd = VE - Bd - pbl * BLd") %>%
  add_equation("Bd = (VE * lambda20) + VE * (lambda22 * rb + lambda23 * ERrbl) + lambda24 * (YDEr)") %>%
  add_equation("BLd = VE * (lambda30 + lambda32 * rb + lambda33 * ERrbl + lambda34 * (YDEr/VE))/pbl") %>%
  add_equation("BLh = BLd", desc = "", hidden = F) %>%
  add_equation("Bs = Bs[-1] + (G + rb[-1] * Bs[-1] + BLs[-1]) - (TX + rb[-1] * Bcb[-1]) - ((BLs - BLs[-1]) * pbl)", desc = "", hidden = F) %>%
  add_equation("Hs = Hs[-1] + (Bcb - Bcb[-1])", desc = "", hidden = F) %>%
  add_equation("Bcb = Bs - Bh", desc = "", hidden = F) %>%
  add_equation("BLs = BLh", desc = "", hidden = F) %>%
  add_equation("ERrbl = rbl + chi * ((pebl - pbl)/pbl)", desc = "", hidden = F) %>%
  add_equation("rbl = 1/pbl", desc = "", hidden = F) %>%
  add_equation("CGE = chi * (pebl - pbl) * BLh", desc = "", hidden = F) %>%
  add_equation("YDEr = YDr[-1]", desc = "", hidden = F) %>%
  add_equation("Bh = Bd", desc = "") %>%
  add_equation("pebl = pbl", desc = "") %>%
  add_equation("Hs = Hh", desc = "Money equilibrium", hidden = TRUE)

# Simulate model
model_lp <- simulate_scenario(model_lp,
  scenario = "baseline", max_iter = 350, periods = 100,
  hidden_tol = 0.1, tol = 1e-08, method = "Gauss"
)

# Plot results
plot_simulation(
  model = model_lp, scenario = c("baseline"), from = 1, to = 100,
  expressions = c("Y", "V")
)
