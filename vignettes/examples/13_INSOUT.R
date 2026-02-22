# A Model with both Inside and Outside Money

# Helper functions for plotting simulation results as deviations from the baseline
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

plotly_style <- function(fig, title = NULL) {
  fig %>%
    plotly::layout(
      title = list(
        text = title %||% "",
        y = 0.95, x = 0.5,
        xanchor = "center", yanchor = "top"
      ),
      margin = list(t = 60, b = 70, l = 60, r = 120),
      hovermode = "spikers",
      spikedistance = 1000,
      xaxis = list(
        title = "time"
      ),
      yaxis = list(
        title = ""
      ),
      showlegend = TRUE,
      legend = list(
        xanchor = "left",
        x = 1.02,
        y = 0.95
      ),
      font = list(
        family = "Arial",
        size = 13
      )
    )
}

do_plotly <- function(m, scenario, variables, t0 = 1,
                      start = NULL, end = NULL,
                      title = NULL,
                      y_title = NULL,
                      y_range = NULL) {
  
  m1 <- m[[scenario]][["result"]]
  
  start <- if (is.null(start)) t0 else start
  end   <- if (is.null(end)) max(m1[["time"]]) else end
  
  # Lookup table: internal variable codes -> figure labels
  lookup_names <- tribble(
    ~name, ~Var,
    "dBhh", "HH. Bills",
    "dV", "Wealth",
    "dM1s", "Checking deposits",
    "dM2s", "Time deposits",
    "dHhs", "Cash",
    "dBLh", "HH. Bonds",
    "yr", "Real output",
    "cr", "Real consumption",
    "ydr", "Real disposable income",
    "c", "Real consumption",
    "v", "Real wealth",
    "rm", "Interest on term deposits",
    "rb", "Interest on bills",
    "rl", "Interest on loans",
    "rbl", "Interest on bonds",
    "dLs", "Loans",
    "dM", "All money deposits",
    "M", "All money deposits",
    "dAs", "CB advances",
    "dHbs", "Reserves",
    "dBbd", "B. Bills",
    "BLRN", "Net bank liq. ratio",
    "BPM", "Bank profit margin",
    "dGb", "Government budget balance",
    "Bcb", "CB. Bills",
    "dPSBR", "Deflated PSBR",
    "pi", "Inflation",
    "gr", "Real govt. budget balance - Book",
    "gr2", "Real govt. budget balance - tav",
    "gr3", "Real govt. budget balance - cb adj.",
    "BYR", "Debt to GDP ratio",
    "dCB", "Change in the CB' stock of bills",
    "ddAs", "Change in Advances to banks",
    "y", "Real output",
    "s", "Real sales"
  )
  
  df_long <- m1 %>%
    mutate(yr = y / y[t0],
           cr = c / c[t0],
           dV = V - V[t0],
           dM1s = M1s - M1s[t0],
           dM2s = M2s - M2s[t0],
           dBhh = Bhh - Bhh[t0],
           dHhs = Hhs - Hhs[t0],
           dBLh = (pbl * BLh) - (pbl[t0] * BLh[t0]),
           dLs = Ls - Ls[t0],
           M = M1s + M2s,
           dM = M - M[t0],
           dAs = As - As[t0],
           dHbs = Hbs - Hbs[t0],
           dBbd = Bbd - Bbd[t0],
           dGb = -1 * (PSBR - PSBR[t0]),
           dPSBR = PSBR / p,
           tav = (p - lag(p)) / p,
           gr = ((p - lag(p)) * (lag(Bs) + lag(BLs) * lag(pbl)) - PSBR) / p,
           gr2 = (tav * (lag(Bs) + lag(BLs) * lag(pbl)) - PSBR) / p,
           gr3 = (tav * (lag(Bs) - lag(Bcb) + lag(BLs) * lag(pbl) - lag(As)) - PSBR)/p,
           BYR = (Bs + pbl * BLs) / Y,
           dCB = Bcb - lag(Bcb),
           ddAs = As - lag(As)
    ) %>%
    filter(time >= start & time <= end) %>%
    select(time, all_of(variables)) %>%
    pivot_longer(cols = -time) %>%
    # Left-join the lookup_names table
    left_join(lookup_names, by = "name") %>%
    mutate(Var = if_else(is.na(Var), name, Var))
  
  fig <- plotly::plot_ly()
  
  for (v in unique(df_long$Var)) {
    dfi <- df_long %>% filter(Var == v)
    
    fig <- plotly::add_trace(
      fig,
      data = dfi,
      x = ~time,
      y = ~value,
      name = v,
      type = "scatter",
      mode = "lines",
      hovertemplate = paste(
        "<b>", v, "</b>",
        "<br>time=%{x}",
        "<br>value=%{y}<extra></extra>"
      )
    )
  }
  
  fig %>%
    plotly::layout(
      yaxis = list(
        title = y_title %||% "",
        range = y_range
      )
    ) %>%
    plotly_style(title)
}

# Create empty model
model_insout <- create_model(name = "SFC INSOUT")

# Add variables
model_insout <- model_insout |>
  add_variable("rbl", init = 0.027) |>
  add_variable("rb", init = 0.023) |>
  add_variable("pr", init = 1) |>
  add_variable("g", init = 25) |>
  add_variable("Nfe", init = 133.28) |>
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
  add_variable("top", init = 0.06) |>
  add_variable("toppm", init = 0.005) |>
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
# Note: Equation numbering follows that used in the text.
model_insout <- model_insout |>
  # Firm's behavioral equations
  add_equation("y = sE + (invE - inv[-1])", desc = "10.1 : y is output, s sales, in inventories (measured as physical objects)") |>
  add_equation("N = y / pr", desc = "10.2 : N is employment, pr productivity") |>
  add_equation("WB = N * W", desc = "10.3 : WB is the wage bill, W the nominal wage rate;") |>
  add_equation("UC = WB / y", desc = "10.4 : UC is the unit cost of producing one object") |>
  add_equation("sE = beta * s[-1] + (1 - beta) * sE[-1]", desc = "10.5 : Adaptative expectations") |>
  add_equation("invT = sigmaT * sE", desc = "10.6 : invT is long-run targeted inventories of widgets") |>
  add_equation("sigmaT = sigma0 - sigma1 * rl", desc = "10.7 : The target inventories to sales ratio depends on rl, the nominal rate of interest on loans") |>
  add_equation("invE = inv[-1] + gamma * (invT - inv[-1])", desc = "10.9 : invE is the short-run planned level of widget inventories") |>
  add_equation("p = (1 + tau) * (1 + phi) * NHUC", desc = "10.10 : NHUC is the normal historic unit cost, defined as:") |>
  add_equation("NHUC = (1 - sigmaT) * UC + sigmaT * (1 + rl[-1]) * UC[-1]", desc = "10.11 : where sigmaT is defined as in (10.7)") |>
  
  # Firm's realized outcomes
  add_equation("s = c + g", desc = "10.12 : Realized sales volume equals consumption plus government expenditures") |>
  add_equation("S = s * p", desc = "10.13 : Realized sales value, in $") |>
  add_equation("inv = inv[-1] + y - s", desc = "10.14 : Realized change in inventories") |>
  add_equation("sigmas = inv[-1] / s", desc = "10.15 : Realized inventories to sales ratio") |>
  add_equation("INV = inv * UC", desc = "10.16 : Realized inventories valued at current unit cost") |>
  add_equation("Ld = INV", desc = "10.17 : Amount of loans required by firms") |>
  add_equation("FXf = S - TX - WB + (INV - INV[-1]) - rl[-1] * INV[-1]", desc = "10.18 : Realized entrepreneurial profits of firms") |>
  add_equation("pi = (p / p[-1]) - 1", desc = "10.19 : pi is the rate of price inflation") |>
  
  # Households – equations describing realized or ex post outcomes
  add_equation("YDr = FX + WB + rm[-1] * M2h[-1] + rb[-1] * Bhh[-1] + BLh[-1]", desc = "10.20 : YDr is the realized nominal regular income of households – the sum of factor income plus interest receipts") |>
  add_equation("CG = (pbl - pbl[-1]) * BLh[-1]", desc = "10.21 : CG is the capital gain on
long-term bonds") |>
  add_equation("YDhs = YDr + CG", desc = "10.22 : YDhs is the Haig–Simons nominal disposable income") |>
  add_equation("FX = FXf + FXb", desc = "10.23 : FXf and FXb are the net profits of firms and of banks") |>
  add_equation("V = V[-1] + YDhs - C", desc = "10.24 : This is the change in realized nominal wealth") |>
  add_equation("Vnc = V - Hhh", desc = "10.25 : Vnc is realized wealth, net of cash") |>
  add_equation("ydr = YDr/p - pi * (V[-1]/p)", desc = "10.26 : ydr is realized real regular disposable income") |>
  add_equation("ydhs = (YDr - pi * V[-1] + CG) / p", desc = "10.27 : ydhs is realized real Haig–Simons income") |>  # The extended version
  add_equation("v = V/p", desc = "10.28 : v is realized real wealth") |>
  
  # Households behavioral
  add_equation("c = alpha0 + alpha1 * ydrE + alpha2 * v[-1]", desc = "10.29 : c is real consumption, that depends on expected real disposable regular income, and past real wealth") |>
  add_equation("ydrE = epsilon * ydr[-1] + (1 - epsilon) * ydrE[-1]", desc = "10.30 : Expected real disposable regular income") |>
  add_equation("C = p * c", desc = "10.31 : C is the $ value of consumptions") |>
  add_equation("YDrE = p * ydrE + pi * (V[-1]/p)", desc = "10.32 : Expected nominal regular disposable income") |>
  add_equation("VE = V[-1] + (YDrE - C)", desc = "10.33 : Expected nominal wealth") |>
  add_equation("Hhd = lambdac * C", desc = "10.34 : Households’ demand for cash") |>
  add_equation("VncE = VE - Hhd", desc = "10.35 : Expected nominal wealth net of cash") |>
  add_equation("ERrbl = rbl", desc = "") |>
  
  # Households' portfolio equations
  add_equation("M2d = VncE * (lambda20 + lambda22 * rm + lambda23 * rb + lambda24 * ERrbl + lambda25 * (YDrE / VncE))", desc = "10.37") |>
  add_equation("Bhd = VncE * (lambda30 + lambda32 * rm + lambda33 * rb + lambda34 * ERrbl + lambda35 * (YDrE / VncE))", desc = "10.38") |>
  add_equation("BLd = (VncE / pbl) * (lambda40 + lambda42 * rm + lambda43 * rb + lambda44 * ERrbl + lambda45 * (YDrE / VncE))", desc = "10.39") |>
  add_equation("M1d = VncE * (lambda10 + lambda12 * rm + lambda13 * rb + lambda14 * ERrbl + lambda15 * (YDrE / VncE))", desc = "10.36") |>
  add_equation("M1d2 = VncE - M2d - Bhd - pbl * BLd", desc = "") |>
  
  # Realized portfolio asset holdings
  add_equation("Hhh = Hhd", desc = "10.40 : intentions regarding cash are fulfilled") |>
  add_equation("Bhh = Bhd", desc = "10.41 : intentions regarding bills are fulfilled") |>
  add_equation("BLh = BLd", desc = "10.42 : intentions regarding bonds are fulfilled") |>
  add_equation("M1hN = Vnc - M2d - Bhd - pbl * BLd", desc = "10.43 : The notional amount of bank checking accounts people would find themselves holding") |>
  add_equation("M2hN = M2d", desc = "") |>
  add_equation("z1 = as.numeric(M1hN > 0)", desc = "10.45") |>
  add_equation("M1h = M1hN * z1", desc = "10.44") |>
  # These two equations ((10.44) and (10.45)) say that the bank checking deposits held are zero if they would turn out to be negative according to equation (10.43);
  # if checking deposits were to be negative, households would adjust them back to zero by decreasing time deposits (see (10.46) and (10.47)).
  add_equation("z2 = 1 - z1", desc = "10.47") |>
  add_equation("M2h = M2d * z1 + (Vnc - Bhh - pbl * BLd) * z2", desc = "10.46") |>
  
  # Government's equations
  add_equation("TX = S * (tau / (1 + tau))", desc = "10.48 : Realized tax revenue from sales tax") |>
  add_equation("G = p * g", desc = "10.49 : Nominal and real pure government expenditures") |>
  add_equation("PSBR = G + rb[-1] * Bs[-1] + BLs[-1] - (TX + FXcb)", desc = "10.50 : Government deficit") |>
  add_equation("Bs = Bs[-1] + PSBR - (BLs - BLs[-1]) * pbl", desc = "10.51 : New issues of bills") |>
  add_equation("BLs = BLd", desc = "10.52 : Bonds are supplied on demand") |>
  add_equation("pbl = 1 / rbl", desc = "10.53 : The price of long-term bonds is the inverse of their yield") |>
  add_equation("GD = GD[-1] + PSBR", desc = "") |>
  
  # The central bank's equations
  add_equation("Hs = Bcb + As", desc = "10.55 : the balance sheet of the central bank") |>
  add_equation("Hbs = Hs - Hhs", desc = "10.56 : the supply of cash (HPM) has two components: the supply to banks and the supply to households") |>
  add_equation("Bcb = Bs - Bhh - Bbd", desc = "10.57 : the central bank is the residual purchaser of bills") |>
  add_equation("As = Ad", desc = "10.59 : Advances to commercial banks are provided on demand") |>
  add_equation("ra = rb", desc = "10.60 : For simplification, the rate on advances is the same as the rate on Treasury bills") |>
  add_equation("FXcb = rb[-1] * Bcb[-1] + ra[-1] * As[-1]", desc = "10.61 : The profits of the central bank") |>
  
  # The duties of the commercial banks - bank's realized (supply) equations
  add_equation("Hhs = Hhd", desc = "10.62 : cash supplied on demand") |>
  add_equation("M1s = M1h", desc = "10.63 : checking deposits supplied on demand") |>
  add_equation("M2s = M2h", desc = "10.64 : time deposits supplied on demand") |>
  add_equation("Ls = Ld", desc = "10.65 : loans supplied on demand") |>
  add_equation("Hbd = ro1 * M1s + ro2 * M2s", desc = "10.66 : the reserve requirements of banks") |>
  
  # The balance-sheet constraint of commercial banks
  add_equation("BbdN = M1s + M2s - Ls - Hbd", desc = "10.67 : Notional balance-sheet constraint of banks;") |>
  add_equation("BLRN = BbdN / (M1s + M2s)", desc = "10.68 : Net bank liquidity ratio") |>
  add_equation("Ad = (bot * (M1s + M2s) - BbdN) * z3", desc = "10.69 : Advances needed by banks") |>  # z3 instead of z4
  add_equation("z3 = as.numeric(BLRN < bot)", desc = "10.70") |>  # z3 instead of z4
  add_equation("Bbd = Ad + M1s + M2s - Ls - Hbd", desc = "10.71 : Actual balance-sheet constraint of banks") |>
  add_equation("BLR = Bbd / (M1s + M2s)", desc = "10.72 : Actual (or gross) bank liquidity ratio") |>
  
  # The determination of interest rates set by banks
  add_equation("rm = rm[-1] + zetam * (z4 - z5) + zetab * (rb - rb[-1])", desc = "10.73-10.74 : Deposit rates move with bill rates and also depend on whether the BLRN is within its target range") |>
  add_equation("z4 = as.numeric(BLRN[-1] < bot)", desc = "10.75") |>
  add_equation("z5 = as.numeric(BLRN[-1] > top)", desc = "10.76") |>
  add_equation("FXb = rl[-1] * Ls[-1] + rb[-1] * Bbd[-1] - rm[-1] * M2s[-1] - ra[-1] * Ad[-1]", desc = "10.77 : profits of banks") |>
  add_equation("rl = rl[-1] + zetal * (z6 - z7) + (rb - rb[-1])", desc = "10.78-10.79 : loan rates move with bill rates and also depend on whether bank profitability is within its target range") |>
  add_equation("z6 = as.numeric(BPM < botpm)", desc = "10.80") |>
  add_equation("z7 = as.numeric(BPM > toppm)", desc = "10.81") |>
  add_equation("lM1s = M1s[-1]", desc = "") |>
  add_equation("lM2s = M2s[-1]", desc = "") |>
  add_equation("BPM = (FXb + FXb[-1]) / (lM1s + lM1s[-1] + lM2s + lM2s[-1])", desc = "10.82 : Mean profit margin of banks") |>
  # Inflationary forces
  add_equation("omegaT = exp(Omega0 + Omega1 * log(pr) + Omega2 * log(N / Nfe))", desc = "10.84") |>
  add_equation("W = W[-1] * (1 + Omega3 * (omegaT[-1] - (W[-1] / p[-1])))", desc = "10.85") |>
  add_equation("Y = p * s + UC * (inv - inv[-1])", desc = "10.86") |>
  
  # Hidden equation
  add_equation("Hbd = Hbs", hidden = TRUE, desc = "10.83A : the redundant equation: supplies of reserves are found to be equal to demand")


# Simulate the baseline scenario
model_insout <- simulate_scenario(model_insout, scenario = "baseline",
                                  max_iter = 350, periods = 210, tol = 1e-15,
                                  hidden_tol = 0.1, method = "Broyden")

# Plot results
exprs <- c("Y", "y", "s", "inv", "pi", "Bs", "M1s", "M2s", "V", "INV", "FXf", "FXb")
plots <- purrr::map(exprs,
             ~ plot_simulation(model = model_insout, scenario = "baseline",
                               from = 1, to = 210, expressions = .x
             )
)
plotly::subplot(plots, nrows = 3, shareX = TRUE, titleX = TRUE)

# A steady state from about t = 100 onward.
t0 <- 110

# Simulation 1.: An increase in the targeted inventories to sale ratio
# Initialise the shock
shock_insout <- create_shock()

# Raise the targeted inventories to sale ratio (sigma0)
shock_insout <- add_shock(shock_insout, variable = "sigma0", value = 0.4, start = t0+5, end = t0+70, desc = "")

# Add as a counterfactual scenario...
model_insout <- model_insout |>
  add_scenario(name = "sigma0_shock", origin = "baseline", shock = shock_insout)

# ...and then simulate it
model_insout <- simulate_scenario(model_insout, scenario = "sigma0_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
# Figure 10.1A Evolution of inventories (and hence bank loans), following an increase
# in the target inventories to sales ratio
plot_simulation(model = model_insout, scenario = c("sigma0_shock"),
                from = t0+1, to = t0+70, expressions = c("INV", "Ls"))

# # Figure 10.1B Evolution of real output and real consumption, relative to their initial
# # steady state values, following an increase in the target inventories to sales ratio
# do_plotly(m = model_insout, scenario = "sigma0_shock", variables = c("yr", "cr"), t0=t0, y_range = c(0.980, 1.030), title = "Scenario: sigma0_shock")

# # Figure 10.1C Evolution of household wealth and of its various components, relative
# # to their initial steady state values, during the first periods that follow an increase in
# # the target inventories to sales ratio
# do_plotly(m = model_insout, scenario = "sigma0_shock", variables = c("dV", "dBhh", "dBLh", "dM1s", "dM2s", "dHhs"), t0=t0, start = t0+2, end = t0+10, y_range = c(-1.5, 4.5), title = "Scenario: sigma0_shock")

# Figure 10.1E Evolution of the various components of the balance sheet of commercial
# banks, relative to their initial steady state values, during the first periods that follow
# an increase in the target inventories to sales ratio
do_plotly(m = model_insout, scenario = "sigma0_shock", variables = c("dM", "dLs", "dAs", "dHbs", "dBbd"), t0=t0, start = t0+3, end = t0+10, y_range = c(-1.5, 5.5), title = "Scenario: sigma0_shock")

# Simulation 2: An increase in pure government expenditure
# Similarly, define the fiscal shock path
shock_insout <- create_shock() |>
  add_shock(variable = "g", value = 30, start = t0+5, end = t0+55, desc = "")

# Add as an alternative scenario
model_insout <- model_insout |>
  add_scenario(name = "g_shock", origin = "baseline", shock = shock_insout)

# Then, simulate the shock
model_insout <- simulate_scenario(model_insout, scenario = "g_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-15, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# And plot the results
# Figure 10.2A Evolution of household real wealth, real disposable income and
# real consumption, following a one-step permanent increase in real government
# expenditures
do_plotly(m = model_insout, scenario = "g_shock", 
        variables = c("c", "v", "ydr"), t0=t0, end = t0+55, title = "Scenario: g_shock")

# # Figure 10.2E Evolution of the debt to GDP ratio, following a one-step permanent
# # increase in real government expenditures
# do_plotly(m = model_insout, scenario = "g_shock",
#           variables = c("BYR"), t0=t0, end = t0+55, y_range = c(0.59, 0.7), title = "Scenario: g_shock")

# # Figure 10.2G Evolution of the various components of the balance sheet of private
# # banks, relative to their initial steady state values, during the first periods that follow
# # an increase in the real government expenditures
# do_plotly(m = model_insout, scenario = "g_shock",
#           variables = c("dBbd", "dM1s", "dM2s", "dLs", "dHbs"), t0=t0, start = t0+3, end = t0+10, y_range = c(-6.5, 9.5), title = "Scenario: g_shock")

# Simulation 5: A decrease in the propensity to consume out of real disposable income
# Again, define, add, and simulate the behavioural shock
shock_insout <- create_shock() |>
  add_shock(variable = "alpha1", value = 0.8, start = t0+5, end = t0+70, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "alpha1_shock", origin = "baseline", shock = shock_insout)

model_insout <- simulate_scenario(model_insout, scenario = "alpha1_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Finally, plot the results
# Figure 10.5A Evolution of real regular disposable income and of real consumption,
# following a decrease in the propensity to consume out of (expected) real regular
# disposable income
do_plotly(
  m = model_insout, scenario = "alpha1_shock",
  variables = c("ydr", "c"), t0 = t0, end = t0+70,
  title = "Scenario: alpha1_shock", y_title = ""
)

# Note: The same procedure applies to all subsequent scenarios; comments are therefore kept to a minimum.

# Simulation 6: An exogenous increase in the rate of inflation
shock_insout <- create_shock() |>
  add_shock(variable = "Omega0", value = -0.2, start = t0+5, end = t0+70, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "Omega0_shock", origin = "baseline", shock = shock_insout)

model_insout <- simulate_scenario(model_insout, scenario = "Omega0_shock", periods = t0+70,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Results
# Figure 10.6B Evolution of real sales and real output following a one-step increase in
# the target real wage that generates an increase in the rate of inflation
do_plotly(m = model_insout, scenario = "Omega0_shock",
          variables = c("y", "s"), t0=t0, end = t0+55, title = "Scenario: Omega0_shock")

# Simulation 7: Increase in the target real wage followed by an increase in interest rates
shock_insout <- create_shock() |>
  add_shock(variable = "Omega0", value = -0.2, start = t0+4, end = t0+55, desc = "") |>
  add_shock(variable = "rb", value = 0.03, start = t0+5, end = t0+55, desc = "") |>
  add_shock(variable = "rbl", value = 0.039, start = t0+5, end = t0+55, desc = "")

model_insout <- model_insout |>
  add_scenario(name = "Omega0-rb(l)_shock", origin = "baseline", shock = shock_insout)

model_insout <- simulate_scenario(model_insout, scenario = "Omega0-rb(l)_shock", periods = t0+55,
                                  max_iter = 350, tol = 1e-30, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Results
# Figure 10.7A Evolution of real sales and real output following a one-step increase in
# the target real wage that generates an increase in the rate of inflation, accompanied by
# an increase in nominal interest rates that approximately compensates for the increase
# in inflation
do_plotly(m = model_insout, scenario = "Omega0-rb(l)_shock",
        variables = c("s"), t0=t0, end = t0+55, title = "Scenario: Omega0, rb and rbl shocks")

# Figure 10.7B Evolution of real household debt and real government debt following
# a one-step increase in the target real wage that generates an increase in the rate of
# inflation, accompanied by an increase in nominal interest rates that approximately
# compensates for the increase in inflation
df_long <- model_insout[["Omega0-rb(l)_shock"]][["result"]] %>%
  mutate(
    `Deflated government debt` = (Bs + pbl * BLs) / p,
    `Real wealth` = v
  ) %>%
  filter(time >= t0 & time <= t0 + 55) %>%
  select(time, `Real wealth`, `Deflated government debt`) %>%
  pivot_longer(cols = -time, names_to = "name", values_to = "value")

fig <- plotly::plot_ly()

for (v in unique(df_long$name)) {
  dfi <- df_long %>% filter(name == v)
  
  fig <- plotly::add_trace(
    fig,
    data = dfi,
    x = ~time,
    y = ~value,
    name = v,
    type = "scatter",
    mode = "lines",
    line = list(width = 2),
    hovertemplate = paste(
      "<b>", v, "</b>",
      "<br>time=%{x}",
      "<br>value=%{y}<extra></extra>"
    )
  )
}

fig %>%
  plotly::layout(
    yaxis = list(title = ""),
    colorway = RColorBrewer::brewer.pal(3, "Dark2")
  ) %>%
  plotly_style("Scenario Omega0, rb and rbl shocks")
