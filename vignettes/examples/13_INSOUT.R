# model INSOUT

# Configuration
if (!require(here)) install.packages("here")
here::i_am("godley/vignettes/examples/13_INSOUT.R")
source(here::here("godley/vignettes/examples/dependencies/00_insout.R"))

# Create empty model
model_insout <- create_model(name = "SFC INSOUT")

# Add variables
model_insout <- model_insout |>
  # EXOGENOUS
  add_variable("rbl", init = 0.027) |>
  add_variable("rb", init = 0.023) |>
  add_variable("pr", init = 1) |>
  add_variable("g", init = 25) |>
  add_variable("Nfe", init = 133.28) |>   # Zezza supplies this exogenous full employment value
  # In model DISINF Nfe was defined as `s / pr`, but it doesn't work here.
  # I discuss this issue in the scenarios.
  # PARAMETERS
  add_variable("alpha0", init = 0) |>
  add_variable("alpha1", init = 0.95) |>
  add_variable("alpha2", init = 0.05) |>
  add_variable("beta", init = 0.5) |>
  add_variable("bot", init = 0.02) |>
  add_variable("botpm", init = 0.003) |>
  add_variable("epsilon", init = 0.5) |>
  add_variable("gamma", init = 0.5) |>
  add_variable("lambdac", init = 0.1) |>
  add_variable("phi", init = 0.1) |>
  add_variable("ro1", init = 0.1) |>
  add_variable("ro2", init = 0.1) |>
  add_variable("sigma0", init = 0.3612) |>
  add_variable("sigma1", init = 3) |>
  add_variable("tau", init = 0.25) |>
  add_variable("zetab", init = 0.9) |>
  add_variable("zetal", init = 0.0002) |>
  add_variable("zetam", init = 0.0002) |>
  add_variable("Omega0", init = -0.32549) |>
  add_variable("Omega1", init = 1) |>
  add_variable("Omega2", init = 1.5) |>
  add_variable("Omega3", init = 0.1) |>
  add_variable("top", init = 0.06) |>     # top = 0.04, # Zezza's top
  add_variable("toppm", init = 0.005) |>
  
  # PORTFOLIO PARAMETERS
  add_variable("lambda10", init = -0.17071) |>
  add_variable("lambda11", init = 0) |>
  add_variable("lambda12", init = 0) |>
  add_variable("lambda13", init = 0) |>
  add_variable("lambda14", init = 0) |>
  add_variable("lambda15", init = 0.18) |>
  add_variable("lambda20", init = 0.52245) |>
  add_variable("lambda21", init = 0) |>
  add_variable("lambda22", init = 30) |>
  add_variable("lambda23", init = -15) |>
  add_variable("lambda24", init = -15) |>
  add_variable("lambda25", init = -0.06) |>
  add_variable("lambda30", init = 0.47311) |>
  add_variable("lambda31", init = 0) |>
  add_variable("lambda32", init = -15) |>
  add_variable("lambda33", init = 30) |>
  add_variable("lambda34", init = -15) |>
  add_variable("lambda35", init = -0.06) |>
  add_variable("lambda40", init = 0.17515) |>
  add_variable("lambda41", init = 0) |>
  add_variable("lambda42", init = -15) |>
  add_variable("lambda43", init = -15) |>
  add_variable("lambda44", init = 30) |>
  add_variable("lambda45", init = -0.06) |>
  
  # Add variables and equations used in the sfcr model but not explicitly declared
  add_variable("y") |>
  add_variable("sE") |>
  add_variable("invE") |>
  add_variable("inv") |>
  add_variable("N") |>
  add_variable("WB") |>
  add_variable("W", init = 1) |>
  add_variable("UC", init = 1) |>
  add_variable("s") |>
  add_variable("invT") |>
  add_variable("sigmaT") |>
  add_variable("rl", init = 0.02) |>
  add_variable("p", init = 1) |>
  add_variable("NHUC") |>
  add_variable("c") |>
  add_variable("S") |>
  add_variable("sigmas") |>
  add_variable("INV") |>
  add_variable("Ld") |>
  add_variable("FXf") |>
  add_variable("TX") |>
  add_variable("pi") |>
  add_variable("YDr") |>
  add_variable("FX") |>
  add_variable("rm", init = 0.02) |>
  add_variable("M2h") |>
  add_variable("Bhh") |>
  add_variable("BLh") |>
  add_variable("CG") |>
  add_variable("pbl") |>
  add_variable("YDhs") |>
  add_variable("FXb") |>
  add_variable("V") |>
  add_variable("C") |>
  add_variable("Vnc") |>
  add_variable("Hhh") |>
  add_variable("ydr") |>
  add_variable("ydhs") |>
  add_variable("v") |>
  add_variable("ydrE") |>
  add_variable("YDrE") |>
  add_variable("VE") |>
  add_variable("Hhd") |>
  add_variable("VncE") |>
  add_variable("ERrbl") |>
  add_variable("M2d") |>
  add_variable("Bhd") |>
  add_variable("BLd") |>
  add_variable("M1d") |>
  add_variable("M1d2") |>
  add_variable("M1hN") |>
  add_variable("z1") |>
  add_variable("z2") |>
  add_variable("M1h") |>
  add_variable("M2hN") |>
  add_variable("G") |>
  add_variable("PSBR") |>
  add_variable("Bs") |>
  add_variable("BLs") |>
  add_variable("FXcb") |>
  add_variable("GD") |>
  add_variable("Hs") |>
  add_variable("Bcb") |>
  add_variable("As") |>
  add_variable("Hbs") |>
  add_variable("Hhs") |>
  add_variable("Bbd") |>
  add_variable("Ad") |>
  add_variable("ra") |>
  add_variable("M1s") |>
  add_variable("M2s") |>
  add_variable("Ls") |>
  add_variable("Hbd") |>
  add_variable("BbdN") |>
  add_variable("BLRN") |>
  add_variable("z3") |>
  add_variable("BLR") |>
  add_variable("z4") |>
  add_variable("z5") |>
  add_variable("z6") |>
  add_variable("z7") |>
  add_variable("BPM", init = 0.0035) |>
  add_variable("lM1s") |>
  add_variable("lM2s") |>
  add_variable("omegaT") |>
  add_variable("Y")

# Add equations
model_insout <- model_insout |>
  # Firm's behavioral equations
  add_equation("y = sE + (invE - inv[-1])", desc = "") |>
  add_equation("N = y / pr", desc = "") |>
  add_equation("WB = N * W", desc = "") |>
  add_equation("UC = WB / y", desc = "") |>
  add_equation("sE = beta * s[-1] + (1 - beta) * sE[-1]", desc = "") |>
  add_equation("invT = sigmaT * sE", desc = "") |>
  add_equation("sigmaT = sigma0 - sigma1 * rl", desc = "") |>
  # rrl ~ ((1 + rl) / (1 + pi)) - 1,
  add_equation("invE = inv[-1] + gamma * (invT - inv[-1])", desc = "") |>
  add_equation("p = (1 + tau) * (1 + phi) * NHUC", desc = "") |>
  add_equation("NHUC = (1 - sigmaT) * UC + sigmaT * (1 + rl[-1]) * UC[-1]", desc = "") |>  # rl[-1] instead of rl
  # FXfE ~ (phi / (1 + phi)) * (1 / (1 + tau)) * p * sE
  
  # Firm's realized outcomes
  add_equation("s = c + g", desc = "") |>
  add_equation("S = s * p", desc = "") |>
  add_equation("inv = inv[-1] + y - s", desc = "") |>
  add_equation("sigmas = inv[-1] / s", desc = "") |>
  add_equation("INV = inv * UC", desc = "") |>
  add_equation("Ld = INV", desc = "") |>
  add_equation("FXf = S - TX - WB + (INV - INV[-1]) - rl[-1] * INV[-1]", desc = "") |>  # rl[-1] instead of rl
  add_equation("pi = (p / p[-1]) - 1", desc = "") |>
  
  # Households realized outcomes
  add_equation("YDr = FX + WB + rm[-1] * M2h[-1] + rb[-1] * Bhh[-1] + BLh[-1]", desc = "") |>  # Here's a mistake in Zezza's code. It should be M2h[-1] as in G&L and NOT M2d.
  add_equation("CG = (pbl - pbl[-1]) * BLh[-1]", desc = "") |>
  add_equation("YDhs = YDr + CG", desc = "") |>
  add_equation("FX = FXf + FXb", desc = "") |>
  add_equation("V = V[-1] + YDhs - C", desc = "") |>
  add_equation("Vnc = V - Hhh", desc = "") |>  # Zezza writes it as Hhd instead of Hhh. Here it is harmless, but theoretically it should be Hhh and not Hhd as what matters for wealth net of cash is the realized holdings of cash.
  add_equation("ydr = YDr/p - pi * (V[-1]/p)", desc = "") |>
  add_equation("ydhs = (YDr - pi * V[-1] + CG) / p", desc = "") |>  # Equation 10.27A: ydhs = c + v - v[-1]
  add_equation("v = V/p", desc = "") |>
  
  # Households behavioral
  add_equation("c = alpha0 + alpha1 * ydrE + alpha2 * v[-1]", desc = "") |>
  add_equation("ydrE = epsilon * ydr[-1] + (1 - epsilon) * ydrE[-1]", desc = "") |>
  add_equation("C = p * c", desc = "") |>
  add_equation("YDrE = p * ydrE + pi * (V[-1]/p)", desc = "") |>
  add_equation("VE = V[-1] + (YDrE - C)", desc = "") |>
  add_equation("Hhd = lambdac * C", desc = "") |>
  add_equation("VncE = VE - Hhd", desc = "") |>
  add_equation("ERrbl = rbl", desc = "") |>  # ERrbl is not on the list of equations. I kept it simple.
  
  # Households' portfolio equations
  # There's no M1d equation in Zezza's code as they are not necessary
  add_equation("M2d = VncE * (lambda20 + lambda22 * rm + lambda23 * rb + lambda24 * ERrbl + lambda25 * (YDrE / VncE))", desc = "") |>
  add_equation("Bhd = VncE * (lambda30 + lambda32 * rm + lambda33 * rb + lambda34 * ERrbl + lambda35 * (YDrE / VncE))", desc = "") |>
  add_equation("BLd = (VncE / pbl) * (lambda40 + lambda42 * rm + lambda43 * rb + lambda44 * ERrbl + lambda45 * (YDrE / VncE))", desc = "") |>
  # However, it is a good exercise to write them down. If all values are correct, the equations below must be equal.
  add_equation("M1d = VncE * (lambda10 + lambda12 * rm + lambda13 * rb + lambda14 * ERrbl + lambda15 * (YDrE / VncE))", desc = "") |>
  add_equation("M1d2 = VncE - M2d - Bhd - pbl * BLd", desc = "") |>
  
  # Realized portfolio asset holdings
  # add_equation("Bhs = Bhd", desc = "") |>  # Not explicit in GL
  add_equation("Hhh = Hhd", desc = "") |>
  add_equation("Bhh = Bhd", desc = "") |>
  add_equation("BLh = BLd", desc = "") |>
  add_equation("M1hN = Vnc - M2d - Bhd - pbl * BLd", desc = "") |>
  add_equation("z1 = as.numeric(M1hN > 0)", desc = "") |>
  add_equation("z2 = 1 - z1", desc = "") |>
  add_equation("M1h = M1hN * z1", desc = "") |>
  add_equation("M2hN = M2d", desc = "") |>
  add_equation("M2h = M2d * z1 + (Vnc - Bhh - pbl * BLd) * z2", desc = "") |>
  
  # Government's equations
  add_equation("TX = S * (tau / (1 + tau))", desc = "") |>
  add_equation("G = p * g", desc = "") |>
  add_equation("PSBR = G + rb[-1] * Bs[-1] + BLs[-1] - (TX + FXcb)", desc = "") |>
  add_equation("Bs = Bs[-1] + PSBR - (BLs - BLs[-1]) * pbl", desc = "") |>
  add_equation("BLs = BLd", desc = "") |>
  add_equation("pbl = 1 / rbl", desc = "") |>
  add_equation("GD = GD[-1] + PSBR", desc = "") |>  # Not in the equations list, but I added to check BS consistency
  
  # Central bank's equations
  add_equation("Hs = Bcb + As", desc = "") |>
  add_equation("Hbs = Hs - Hhs", desc = "") |>
  add_equation("Bcb = Bs - Bhh - Bbd", desc = "") |>
  add_equation("As = Ad", desc = "") |>
  add_equation("ra = rb", desc = "") |>
  add_equation("FXcb = rb[-1] * Bcb[-1] + ra[-1] * As[-1]", desc = "") |>
  
  # Bank's realized (supply) equations
  add_equation("Hhs = Hhd", desc = "") |>
  add_equation("M1s = M1h", desc = "") |>   # M1h instead of M1d as in Zezza. Bank's supply the realized portfolio holdings and NOT its notional demand.
  add_equation("M2s = M2h", desc = "") |>   # M2h instead of M2d as in Zezza. Bank's supply the realized portfolio holdings and NOT its notional demand.
  # Otherwise, the model would not close because M1d depends on EXPECTED wealth but the REALIZED demand for deposits depends on REALIZED wealth. It is a residual variable. The bank's supply the deposits that are ACTUALLY needed. You can check that M1d, however measured, only equals M1h in the stationary state.
  add_equation("Ls = Ld", desc = "") |>
  add_equation("Hbd = ro1 * M1s + ro2 * M2s", desc = "") |>
  
  # Bank's balance sheet constraints
  add_equation("BbdN = M1s + M2s - Ls - Hbd", desc = "") |>
  add_equation("BLRN = BbdN / (M1s + M2s)", desc = "") |>
  add_equation("Ad = (bot * (M1s + M2s) - BbdN) * z3", desc = "") |>  # z3 instead of z4
  add_equation("z3 = as.numeric(BLRN < bot)", desc = "") |>
  add_equation("Bbd = Ad + M1s + M2s - Ls - Hbd", desc = "") |>
  add_equation("BLR = Bbd / (M1s + M2s)", desc = "") |>
  
  # Determination of interest rates by banks
  add_equation("rm = rm[-1] + zetam * (z4 - z5) + zetab * (rb - rb[-1])", desc = "") |>
  add_equation("z4 = as.numeric(BLRN[-1] < bot)", desc = "") |>
  add_equation("z5 = as.numeric(BLRN[-1] > top)", desc = "") |>
  add_equation("FXb = rl[-1] * Ls[-1] + rb[-1] * Bbd[-1] - rm[-1] * M2s[-1] - ra[-1] * Ad[-1]", desc = "") |>
  add_equation("rl = rl[-1] + zetal * (z6 - z7) + (rb - rb[-1])", desc = "") |>
  add_equation("z6 = as.numeric(BPM < botpm)", desc = "") |>
  add_equation("z7 = as.numeric(BPM > toppm)", desc = "") |>
  # Since the sfcr package does not accept more than 1 lag directly, we need to create two auxiliary values that are lags of endogenous variables and then take the lag of these variables.
  add_equation("lM1s = M1s[-1]", desc = "") |>
  add_equation("lM2s = M2s[-1]", desc = "") |>
  add_equation("BPM = (FXb + FXb[-1]) / (lM1s + lM1s[-1] + lM2s + lM2s[-1])", desc = "") |>
  # Inflationary forces
  # omegaT = Omega0 + Omega1 * pr + Omega2 * (N / Nfe)
  add_equation("omegaT = exp(Omega0 + Omega1 * log(pr) + Omega2 * log(N / Nfe))", desc = "") |>  # Zezza's equation
  
  # The model explodes if G&L's definition of omegaT is used.
  add_equation("W = W[-1] * (1 + Omega3 * (omegaT[-1] - (W[-1] / p[-1])))", desc = "") |>
  # add_equation("Nfe = s / pr", desc = "") |>  # Zezza's definition in model DISINF. Doesn't work here.
  # add_equation("Nfe = s[-1] / pr", desc = "") |>  # One possible solution
  add_equation("Y = p * s + UC * (inv - inv[-1])", desc = "") |>
  
  # Hidden equation
  add_equation("Hbd = Hbs", hidden = TRUE)

# Simulate model
model_insout <- simulate_scenario(model_insout, scenario = "baseline",
                                  max_iter = 350, periods = 210, tol = 1e-15,
                                  hidden_tol = 0.1, method = "Broyden")

# Steady state
exprs <- c("Y", "y", "s", "inv", "pi", "Bs", "M1s", "M2s", "V", "INV", "FXf", "FXb")  # Bs, M2s, FXb look different
plots <- map(exprs,
             ~ plot_simulation(model = model_insout, scenario = "baseline",
                               from = 1, to = 210, expressions = .x
             )
)
subplot(plots, nrows = 3, shareX = TRUE, titleX = TRUE)

# So far, so good. But what about the variables that depend on bot, top, botpm, toppm? We need to check that the margins are not too tight.
plot_simulation(model = model_insout, scenario = c("baseline"),
                from = 35, to = 210, expressions = c("BLRN", "bot", "top"))

# What about BPM and toppm and botpm?
plot_simulation(model = model_insout, scenario = c("baseline"),
                from = 10, to = 210, expressions = c(c("BPM", "toppm", "botpm")))

# Let’s take a look at the structure of this model:
plot_cycles(model_insout)

# A steady state from about t = 100 onward.
t0 <- 100

# Simulation 1: An increase in the targeted inventories to sale ratio
# Create empty shock and add shock equation
shock_insout <- create_shock() |>
  add_shock(variable = "sigma0", value = 0.4, start = t0+5, end = t0+70, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "sigma0_shock", origin = "baseline", shock = shock_insout)

# Simulate shock
model_insout <- simulate_scenario(model_insout, scenario = "sigma0_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
# Figure 11.2A
plot_simulation(model = model_insout, scenario = c("baseline", "sigma0_shock"),
                from = t0+1, to = t0+70, expressions = c("INV", "Ls"))

# Evolution of real consumption and output
do_plot(m = model_insout, scenario = "sigma0_shock", variables = c("yr", "cr"), t0=t0) +
labs(title = "INSOUT1",
     subtitle = "Evolution of real consumption and output")

# Evolution of household wealth and its components
do_plot(m = model_insout, scenario = "sigma0_shock",
        variables = c("dV", "dBhh", "dBLh", "dM1s", "dM2s", "dHhs"),
        t0 = t0, start = t0+2, end = t0+10) +
  labs(title = "INSOUT1",
       subtitle = "Evolution of household wealth and its components")

# Evolution of short-term interest rates
do_plot(m = model_insout, scenario = "sigma0_shock", variables = c("rb", "rm"), t0=t0) +
  labs(title = "INSOUT1",
       subtitle = "Evolution of short-term interest rates")
  
# Evolution of banks' balance sheet
do_plot(m = model_insout, scenario = "sigma0_shock",
        variables = c("dM", "dLs", "dAs", "dHbs", "dBbd"),
        t0 = t0, start = t0+3, end = t0+10) +
    labs(title = "INSOUT1",
         subtitle = "Evolution of banks' balance sheet")

# Evolution of net bank liquidity ratio
do_plot(m = model_insout, scenario = "sigma0_shock", variables = c("BLRN", "bot", "top"), t0=t0) +
  labs(title = "INSOUT1",
       subtitle = "Evolution of net bank liquidity ratio")

# Evolution of the bank profitability margin
do_plot(m = model_insout, scenario = "sigma0_shock", variables = c("BPM", "toppm", "botpm"), t0=t0) +
  labs(title = "INSOUT1",
       subtitle = "Evolution of the bank profitability margin")

# Evolution of the government budget balance
do_plot(m = model_insout, scenario = "sigma0_shock", variables = c("dGb"), t0=t0) +
  theme(legend.position = "bottom") +
  labs(title = "INSOUT1",
       subtitle = "Evolution of the government budget balance")

# Evolution of the stock of treasury bills hend by the central bank
do_plot(m = model_insout, scenario = "sigma0_shock", variables = c("Bcb"), t0=t0) +
  theme(legend.position = "bottom") +
  labs(title = "INSOUT1",
       subtitle = "Evolution of the stock of treasury bills hend by the central bank")


# Evolution of short-term interest rates
# Sensitivity test to show that there’s nothing in the model that prevents the interest rate on deposits to become higher than the interest rate on bills
model_sen <- model_insout %>%
  create_sensitivity(
    variable = "sigma0", lower = 0.37, upper = 0.55, step = 0.02
  ) %>%
  simulate_scenario(periods = 80)

plot_simulation(model_sen, scenario = "sensitivity", take_all = T,
                # rb - interest on bills, rm - interest on term deposits
                from = 20, expressions = c("rm", "rb"))

# Simulation 2: An increase in pure government expenditure
# Create empty shock and add shock equation
shock_insout <- create_shock() |>
  add_shock(variable = "g", value = 30, start = t0+5, end = t0+55, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "g_shock", origin = "baseline", shock = shock_insout)

# Simulate shock
model_insout <- simulate_scenario(model_insout, scenario = "g_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-15, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
# Evolution of HH. real consumption, disp. income, and wealth
do_plot(m = model_insout, scenario = "g_shock", 
        variables = c("c", "v", "ydr"), t0=t0, end = t0+55) +
  labs(title = "INSOUT2",
       subtitle = "Evolution of HH. real consumption, disp. income, and wealth")

# Evolution of deflated PSBR
do_plot(m = model_insout, scenario = "g_shock", 
        variables = c("dPSBR", "PSBR"), t0=t0, end = t0+55) +
  labs(title = "INSOUT2",
       subtitle = "Evolution of deflated PSBR")

# Evolution of the price inflation rate
do_plot(m = model_insout, scenario = "g_shock", 
        variables = c("pi"), t0=t0, end = t0+55) +
  labs(
    title = "INSOUT2",
    subtitle = "Evolution of the price inflation rate"
  )

# Evolution of the real budget balance
do_plot(m = model_insout, scenario = "g_shock", 
        variables = c("gr", "gr2", "gr3"), t0=t0, end = t0+55) +
  labs(title = "INSOUT2",
       subtitle = "Evolution of the real budget balance",
       caption = "Takes into account the capital gains due to the erosion of the public debt\n by price inflation.")

# 
do_plot(m = model_insout, scenario = "g_shock", variables = c("BYR"), t0=t0, end = t0+55) +
  labs(title = "INSOUT2")

# Evolution of interest rates
do_plot(m = model_insout, scenario = "g_shock", 
        variables = c("rl", "rm", "rb", "rbl"), t0=t0, end = t0+55) +
  labs(title = "INSOUT2",
       subtitle = "Evolution of interest rates")

# Evolution of banks' balance sheet
do_plot(m = model_insout, scenario = "g_shock",
        variables = c("dBbd", "dM1s", "dM2s", "dLs", "dHbs"),
        t0 = t0, start = t0+3, end = t0+10) +
  labs(title = "INSOUT2",
       subtitle = "Evolution of banks' balance sheet")

# Evolution of the net bank liquidity ratio
do_plot(m = model_insout, scenario = "g_shock", 
        variables = c("BLRN", "top", "bot"), t0=t0, end = t0+55) +
  labs(title = "INSOUT2",
       subtitle = "Evolution of the net bank liquidity ratio")

# Evolution of the bank profitability margin
do_plot(m = model_insout, scenario = "g_shock", 
        variables = c("BPM", "toppm", "botpm"), t0=t0, end = t0+55) +
  labs(title = "INSOUT2",
       subtitle = "Evolution of the bank profitability margin")

# Evolution of the central bank balance sheet
do_plot(m = model_insout, scenario = "g_shock", 
        variables = c("dCB", "ddAs"), t0=t0, end = t0+55) +
  labs(title = "INSOUT2",
       subtitle = "Evolution of the central bank balance sheet")

# Evolution of deflated bills
insout2 <- model_insout[["g_shock"]][["result"]]
insout2 %>%
  mutate(across(c(Bhh, Bs, Bcb, Bbd), ~.x / p)) %>%
  filter(time >= t0 & time <= t0+55) %>%
  select(time, Bcb, Bs, Bhh, Bbd) %>%
  pivot_longer(cols = -time) %>%
  ggplot(aes(x = time, y = value)) +
  geom_line(aes(color = name)) +
  scale_color_brewer("Variable", type = 'qual', palette = "Dark2") +
  labs(title = "INSOUT2",
       subtitle = "Evolution of deflated bills")

# Simulation 3: An increase in the compulsory reserve ratios
# Create empty shock and add shock equation
shock_insout <- create_shock() |>
  add_shock(variable = "ro1", value = 0.2, start = t0+5, end = t0+70, desc = "") |>
  add_shock(variable = "ro2", value = 0.2, start = t0+5, end = t0+70, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "ro1-2_shock", origin = "baseline", shock = shock_insout)

# Simulate shock
model_insout <- simulate_scenario(model_insout, scenario = "ro1-2_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
# Evolution of banks' balance sheet
do_plot(m = model_insout, scenario = "ro1-2_shock",
        variables = c("dM", "dLs", "dAs", "dHbs", "dBbd"),
        t0 = t0, start = t0+2, end = t0+10) +
  labs(title = "INSOUT3",
       subtitle = "Evolution of banks' balance sheet")

# Evolution of net bank liquidity ratio
do_plot(m = model_insout, scenario = "ro1-2_shock", 
        variables = c("top", "bot", "BLRN"), t0=t0, end = t0+70) +
  labs(title = "INSOUT3",
       subtitle = "Evolution of net bank liquidity ratio")

# Evolution of interest rates
do_plot(m = model_insout, scenario = "ro1-2_shock", 
        variables = c("rm", "rb", "rl"), t0=t0, end = t0+70) +
  labs(title = "INSOUT3",
       subtitle = "Evolution of interest rates")

# Evolution of bank profitability margin
do_plot(m = model_insout, scenario = "ro1-2_shock", 
        variables = c("toppm", "botpm", "BPM"), t0=t0, end = t0+70) +
  labs(title = "INSOUT3",
       subtitle = "Evolution of bank profitability margin")

# Evolution of money deposits
do_plot(m = model_insout, scenario = "ro1-2_shock", 
        variables = c("M"), t0=t0, end = t0+70) +
  labs(title = "INSOUT3",
       subtitle = "Evolution of money deposits")

# Simulation 4: An increase in the acceptable bank liquidity ratio
# Create empty shock and add shock equation
shock_insout <- create_shock() |>
  add_shock(variable = "top", value = 0.24, start = t0+5, end = t0+70, desc = "") |>
  add_shock(variable = "bot", value = 0.2, start = t0+5, end = t0+70, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "top-bot_shock", origin = "baseline", shock = shock_insout)

# Simulate shock
model_insout <- simulate_scenario(model_insout, scenario = "top-bot_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
# Evolution of net bank liquidity ratio
do_plot(m = model_insout, scenario = "top-bot_shock",
        variables = c("BLRN"), t0=t0, end = t0+70) +
  map(c(0.03, 0.06, 0.2, 0.24), ~geom_hline(yintercept = .x)) +
  labs(title = "INSOUT4",
       subtitle = "Evolution of net bank liquidity ratio")

# Evolution of interest rates
do_plot(m = model_insout, scenario = "top-bot_shock",
        variables = c("rl", "rb", "rm"), t0=t0, end = t0+70) + 
  labs(title = "INSOUT4",
       subtitle = "Evolution of interest rates")
  
# Evolution of banks' balance sheet
do_plot(m = model_insout, scenario = "top-bot_shock",
        variables = c("dM", "dLs", "dAs", "dHbs", "dBbd"), t0=t0, end = t0+70) +
  labs(title = "INSOUT4",
       subtitle = "Evolution of banks' balance sheet")

# Simulation 5: A decrease in the propensity to consume out of real disposable income
# Create empty shock and add shock equation
shock_insout <- create_shock() |>
  add_shock(variable = "alpha1", value = 0.8, start = t0+5, end = t0+70, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "alpha1_shock", origin = "baseline", shock = shock_insout)

# Simulate shock
model_insout <- simulate_scenario(model_insout, scenario = "alpha1_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
# Evolution of consumption and income
do_plot(m = model_insout, scenario = "alpha1_shock",
        variables = c("ydr", "c"), t0=t0, end = t0+70) +
  labs(title = "INSOUT5",
       subtitle = "Evolution of consumption and income")
  
do_plot(m = model_insout, scenario = "alpha1_shock",
        variables = c("gr2"), t0=t0, end = t0+70) +
  labs(title = "INSOUT5")

# Simulation 6: An exogenous increase in the rate of inflation
# Create empty shock and add shock equation
shock_insout <- create_shock() |>
  add_shock(variable = "Omega0", value = -0.2, start = t0+5, end = t0+70, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "Omega0_shock", origin = "baseline", shock = shock_insout)

# Simulate shock
model_insout <- simulate_scenario(model_insout, scenario = "Omega0_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
do_plot(m = model_insout, scenario = "Omega0_shock",
        variables = c("pi"), t0=t0, end = t0+70) +
  labs(title = "INSOUT6")

# Evolution of output and sales
do_plot(m = model_insout, scenario = "Omega0_shock",
        variables = c("y", "s"), t0=t0, end = t0+70) +
  labs(title = "INSOUT6",
       subtitle = "Evolution of output and sales")

# Evolution of public sector borrowing requirements
do_plot(m = model_insout, scenario = "Omega0_shock",
        variables = c("dPSBR", "PSBR"), t0=t0, end = t0+70) +
  labs(title = "INSOUT6",
       subtitle = "Evolution of public sector borrowing requirements")

do_plot(m = model_insout, scenario = "Omega0_shock",
        variables = c("gr", "gr2"), t0=t0, end = t0+70)

# Simulation 7: Increase in the target real wage followed by an increase in interest rates
# Create empty shock and add shock equation
shock_insout <- create_shock() |>
  add_shock(variable = "Omega0", value = -0.2, start = t0+4, end = t0+55, desc = "") |>
  add_shock(variable = "rb", value = 0.03, start = t0+5, end = t0+55, desc = "") |>
  add_shock(variable = "rbl", value = 0.039, start = t0+5, end = t0+55, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "Omega0-rb(l)_shock", origin = "baseline", shock = shock_insout)

# Simulate shock
model_insout <- simulate_scenario(model_insout, scenario = "Omega0-rb(l)_shock", periods = t0+55,
                                  max_iter = 350, tol = 1e-30, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
do_plot(m = model_insout, scenario = "Omega0-rb(l)_shock",
        variables = c("pi"), t0=t0, end = t0+55) +
  labs(title = "INSOUT7")

do_plot(m = model_insout, scenario = "Omega0-rb(l)_shock",
        variables = c("s"), t0=t0, end = t0+55) +
  labs(title = "INSOUT7")

# Evolution of Real wealth and Deflated government debt
insout7 <- model_insout[["Omega0-rb(l)_shock"]][["result"]]
insout7 %>%
  mutate(
    `Deflated government debt` = (Bs + pbl * BLs) / p,
    `Real wealth` = v
    ) %>%
  filter(time >= t0 & time <= t0+55) %>%
  select(time, "Real wealth", "Deflated government debt") %>%
  pivot_longer(cols = -time) %>%
  ggplot(aes(x = time, y = value)) +
  geom_line(aes(color = name)) +
  scale_color_brewer("Variable", type = 'qual', palette = "Dark2") 

# Evolution of deflated balance and real govt. budget balance - tav
insout7 %>%
  mutate(
    `deflated balance` = -PSBR / p,
    tav = (p - lag(p)) / p,
    gr2 = (tav * (lag(Bs) + lag(BLs) * lag(pbl)) - PSBR) / p,
    `Real govt. budget balance - tav` = gr2
    ) %>%
  filter(time >= t0 & time <= t0+55) %>%
  select(time, "deflated balance", "Real govt. budget balance - tav") %>%
  pivot_longer(cols = -time) %>%
  ggplot(aes(x = time, y = value)) +
  geom_line(aes(color = name)) +
  scale_color_brewer("Variable", type = 'qual', palette = "Dark2") +
  labs(title = "INSOUT7")
