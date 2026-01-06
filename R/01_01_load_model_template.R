# ' Load model template
# '
# ' @param model_template_name string name of model template
# '
# ' @return SFC model object

load_model_template <- function(model_template_name) {
  model <- structure(list(), class = "SFC")

  if (model_template_name == "SIM") {
    # variables
    model <- model |>
      add_variable("C_d", desc = "Consumption demand by households") |>
      add_variable("C_s", desc = "Consumption supply") |>
      add_variable("G_s", desc = "Government supply") |>
      add_variable("H_h", desc = "Cash money held by households") |>
      add_variable("H_s", desc = "Cash money supplied by the government") |>
      add_variable("N_d", desc = "Demand for labor") |>
      add_variable("N_s", desc = "Supply of labor") |>
      add_variable("T_d", desc = "Taxes, demand") |>
      add_variable("T_s", desc = "Taxes, supply") |>
      add_variable("Y", desc = "Income = GDP") |>
      add_variable("Yd", desc = "Disposable income of households")

    # parameters
    model <- model |>
      add_variable("alpha1", init = 0.6, desc = "Propensity to consume out of income") |>
      add_variable("alpha2", init = 0.4, desc = "Propensity to consume out of wealth") |>
      add_variable("theta", init = 0.2, desc = "Tax rate") |>
      add_variable("G_d", init = 20, desc = "Government demand") |>
      add_variable("W", init = 1, desc = "Wage rate")

    # equations
    model <- model |>
      add_equation("C_s = C_d", desc = "Consumption") |>
      add_equation("G_s = G_d") |>
      add_equation("T_s = T_d") |>
      add_equation("N_s = N_d") |>
      add_equation("Yd = W * N_s - T_s") |>
      add_equation("T_d = theta * W * N_s") |>
      add_equation("C_d = alpha1 * Yd + alpha2 * H_h[-1]") |>
      add_equation("H_s = G_d - T_d + H_s[-1]") |>
      add_equation("H_h = Yd - C_d + H_h[-1]") |>
      add_equation("Y = C_s + G_s") |>
      add_equation("N_d = Y/W") |>
      add_equation("H_s = H_h", desc = "Money equilibrium", hidden = TRUE)
  } else if (model_template_name == "PC") {
    # variables
    model <- model |>
      add_variable("B_cb", desc = "") |>
      add_variable("H_s", desc = "") |>
      add_variable("B_s", desc = "") |>
      add_variable("B_h", desc = "") |>
      add_variable("H_h1", desc = "") |>
      add_variable("H_h", desc = "") |>
      add_variable("C", desc = "") |>
      add_variable("V", desc = "") |>
      add_variable("T_x", desc = "") |>
      add_variable("Y", desc = "Income = GDP") |>
      add_variable("Yd", desc = "Disposable income of households")

    # parameters
    model <- model |>
      add_variable("alpha1", init = 0.6, desc = "Propensity to consume out of income") |>
      add_variable("alpha2", init = 0.4, desc = "Propensity to consume out of wealth") |>
      add_variable("theta", init = 0.2, desc = "Tax rate") |>
      add_variable("r", init = 0.025, desc = "") |>
      add_variable("G", init = 20, desc = "Government demand") |>
      add_variable("lambda0", init = 0.635, desc = "") |>
      add_variable("lambda1", init = 0.05, desc = "") |>
      add_variable("lambda2", init = 0.01, desc = "")

    # equations
    model <- model |>
      add_equation("Y = C + G", desc = "") |>
      add_equation("Yd = Y - T_x + r[-1] * B_h[-1]") |>
      add_equation("T_x = theta * (Y + r[-1] * B_h[-1])") |>
      add_equation("V = V[-1] + (Yd - C)") |>
      add_equation("C = alpha1 * Yd + alpha2 * V[-1]") |>
      add_equation("H_h = V - B_h") |>
      add_equation("H_h1 = V * ((1 - lambda0) - lambda1 * r + lambda2 * ( Yd/V ))") |>
      add_equation("B_h = V * (lambda0 + lambda1 * r - lambda2 * ( Yd/V ))") |>
      add_equation("B_s = B_s[-1] + (G + r[-1] * B_s[-1]) - (T_x + r[-1] * B_cb[-1])") |>
      add_equation("H_s = H_s[-1] + B_cb - B_cb[-1]") |>
      add_equation("B_cb = B_s - B_h") |>
      add_equation("H_h = H_s", hidden = T)
  } else if (model_template_name == "PCEX") {
    # variables
    model <- model |>
      add_variable("B_cb", desc = "") |>
      add_variable("H_s", desc = "") |>
      add_variable("B_s", desc = "") |>
      add_variable("B_h", desc = "") |>
      add_variable("H_d1", desc = "") |>
      add_variable("H_d", desc = "") |>
      add_variable("H_h", desc = "") |>
      add_variable("C", desc = "") |>
      add_variable("V", desc = "") |>
      add_variable("T_x", desc = "") |>
      add_variable("Y", desc = "Income = GDP") |>
      add_variable("Yd", desc = "Disposable income of households") |>
      add_variable("Yd_e") |>
      add_variable("B_d") |>
      add_variable("V_e")

    # parameters
    model <- model |>
      add_variable("Ra") |>
      add_variable("alpha1", init = 0.6, desc = "Propensity to consume out of income") |>
      add_variable("alpha2", init = 0.4, desc = "Propensity to consume out of wealth") |>
      add_variable("theta", init = 0.2, desc = "Tax rate") |>
      add_variable("r", init = 0.025, desc = "") |>
      add_variable("G", init = 20, desc = "Government demand") |>
      add_variable("lambda0", init = 0.635, desc = "") |>
      add_variable("lambda1", init = 0.05, desc = "") |>
      add_variable("lambda2", init = 0.01, desc = "")

    # equations
    model <- model |>
      add_equation("Y = C + G", desc = "") |>
      add_equation("Yd = Y - T_x + r[-1] * B_h[-1]") |>
      add_equation("T_x = theta * (Y + r[-1] * B_h[-1])") |>
      add_equation("V = V[-1] + (Yd - C)") |>
      add_equation("C = alpha1 * Yd_e + alpha2 * V[-1]") |>
      add_equation("B_d =  V_e * lambda0 + V_e * lambda1 * r - lambda2 * Yd_e") |>
      add_equation("H_d1 = V_e * (1 - lambda0) - V_e * lambda1 * r + lambda2 * Yd_e") |>
      add_equation("H_d = V_e - B_d") |>
      add_equation("V_e = V[-1] + (Yd_e - C)") |>
      add_equation("H_h = V - B_h") |>
      add_equation("B_h = B_d") |>
      add_equation("B_s = B_s[-1] + (G + r[-1] * B_s[-1]) - (T_x + r[-1] * B_cb[-1])") |>
      add_equation("H_s = H_s[-1] + B_cb - B_cb[-1]") |>
      add_equation("B_cb = B_s - B_h") |>
      add_equation("Yd_e = Yd * (1 + Ra)") |>
      add_equation("Ra = rnorm(1, 0, 0.05)") |>
      add_equation("H_h = H_s", hidden = T)
  } else if (model_template_name == "LP") {
    # variables
    model <- model |>
      add_variable("Y", desc = "") |>
      add_variable("YDr") |>
      add_variable("TX") |>
      add_variable("V", init = 0.0000001) |>
      add_variable("CG") |>
      add_variable("C") |>
      add_variable("VE", init = 0.0000001) |>
      add_variable("Hh") |>
      add_variable("Hd") |>
      add_variable("Bd") |>
      add_variable("BLd") |>
      add_variable("BLh", desc = "") |>
      add_variable("Bs", desc = "") |>
      add_variable("Hs", desc = "") |>
      add_variable("Bcb", desc = "") |>
      add_variable("BLs", desc = "") |>
      add_variable("ERrbl", desc = "") |>
      add_variable("rbl", desc = "") |>
      add_variable("CGE", desc = "") |>
      add_variable("YDEr", desc = "") |>
      add_variable("pebl", desc = "") |>
      add_variable("Bh", desc = "")

    # parameters
    model <- model |>
      add_variable("alpha1", init = 0.8, desc = "Propensity to consume out of income") |>
      add_variable("alpha2", init = 0.2, desc = "Propensity to consume out of wealth") |>
      add_variable("theta", init = 0.1938, desc = "Tax rate") |>
      add_variable("rb", init = 0.03, desc = "") |>
      add_variable("G", init = 20, desc = "Government demand") |>
      add_variable("pbl", init = 20, desc = "") |>
      add_variable("lambda20", init = 0.44196, desc = "") |>
      add_variable("lambda22", init = 1.1, desc = "") |>
      add_variable("lambda23", init = -1, desc = "") |>
      add_variable("lambda24", init = -0.03, desc = "") |>
      add_variable("lambda30", init = 0.3997, desc = "") |>
      add_variable("lambda32", init = -1, desc = "") |>
      add_variable("lambda33", init = 1.1, desc = "") |>
      add_variable("lambda34", init = -0.03, desc = "") |>
      add_variable("chi", init = 0.1, desc = "")

    # equations
    model <- model |>
      add_equation("Y=C + G", desc = "") |>
      add_equation("YDr= Y - TX + rb[-1] * Bh[-1] + BLh[-1]") |>
      add_equation("TX =theta * (Y + rb[-1] * Bh[-1] + BLh[-1])") |>
      add_equation("V = V[-1] + (YDr - C) + CG") |>
      add_equation("CG = (pbl - pbl[-1]) * BLh[-1]") |>
      add_equation("C = alpha1 * YDEr + alpha2 * V[-1]") |>
      add_equation("VE = V[-1] + (YDEr - C) + CG") |>
      add_equation("Hh = V - Bh - pbl * BLh") |>
      add_equation("Hd = VE - Bd - pbl * BLd") |>
      add_equation("Bd = (VE * lambda20) + VE * (lambda22 * rb + lambda23 * ERrbl) + lambda24 * (YDEr)") |>
      add_equation("BLd = VE * (lambda30 + lambda32 * rb + lambda33 * ERrbl + lambda34 * (YDEr/VE))/pbl") |>
      add_equation("BLh = BLd", desc = "", hidden = F) |>
      add_equation("Bs = Bs[-1] + (G + rb[-1] * Bs[-1] + BLs[-1]) - (TX + rb[-1] * Bcb[-1]) - ((BLs - BLs[-1]) * pbl)",
        desc = "", hidden = F
      ) |>
      add_equation("Hs = Hs[-1] + (Bcb - Bcb[-1])", desc = "", hidden = F) |>
      add_equation("Bcb = Bs - Bh", desc = "", hidden = F) |>
      add_equation("BLs = BLh", desc = "", hidden = F) |>
      add_equation("ERrbl = rbl + chi * ((pebl - pbl)/pbl)", desc = "", hidden = F) |>
      add_equation("rbl = 1/pbl", desc = "", hidden = F) |>
      add_equation("CGE = chi * (pebl - pbl) * BLh", desc = "", hidden = F) |>
      add_equation("YDEr = YDr[-1]", desc = "", hidden = F) |>
      add_equation("Bh = Bd", desc = "") |>
      add_equation("pebl = pbl", desc = "") |>
      add_equation("Hs = Hh", desc = "Money equilibrium", hidden = TRUE)
  } else if (model_template_name == "REG") {
    # parameters
    model <- model |>
      add_variable("r", init = 0.025) |>
      add_variable("G_S", init = 20) |>
      add_variable("G_N", init = 20) |>
      add_variable("mu_N", init = 0.15) |>
      add_variable("mu_S", init = 0.15) |>
      add_variable("alpha1_N", init = 0.7) |>
      add_variable("alpha1_S", init = 0.7) |>
      add_variable("alpha2_N", init = 0.3) |>
      add_variable("alpha2_S", init = 0.3) |>
      add_variable("lambda0_N", init = 0.67) |>
      add_variable("lambda0_S", init = 0.67) |>
      add_variable("lambda1_N", init = 0.05) |>
      add_variable("lambda1_S", init = 0.05) |>
      add_variable("lambda2_N", init = 0.01) |>
      add_variable("lambda2_S", init = 0.01) |>
      add_variable("theta", init = 0.2) |>
      add_variable("Y_N") |>
      add_variable("C_N") |>
      add_variable("X_N") |>
      add_variable("IM_N") |>
      add_variable("Y_S") |>
      add_variable("C_S") |>
      add_variable("X_S") |>
      add_variable("IM_S") |>
      add_variable("YD_N") |>
      add_variable("TX_N") |>
      add_variable("Bh_N") |>
      add_variable("YD_S") |>
      add_variable("TX_S") |>
      add_variable("Bh_S") |>
      add_variable("V_N") |>
      add_variable("V_S") |>
      add_variable("Hh_N") |>
      add_variable("Hh_S") |>
      add_variable("TX") |>
      add_variable("G") |>
      add_variable("Bh") |>
      add_variable("Bs") |>
      add_variable("Hh") |>
      add_variable("Hs") |>
      add_variable("Bcb")

    # equations
    model <- model |>
      add_equation("Y_N = C_N + G_N + X_N - IM_N") |>
      add_equation("Y_S = C_S + G_S + X_S - IM_S") |>
      add_equation("IM_N = mu_N * Y_N") |>
      add_equation("IM_S = mu_S * Y_S") |>
      add_equation("X_N = IM_S") |>
      add_equation("YD_N = Y_N - TX_N + r[-1] * Bh_N[-1]") |>
      add_equation("YD_S = Y_S - TX_S + r[-1] * Bh_S[-1]") |>
      add_equation("TX_N = theta * ( Y_N + r[-1] * Bh_N[-1])") |>
      add_equation("X_S = IM_N") |>
      add_equation("TX_S = theta * ( Y_S + r[-1] * Bh_S[-1])") |>
      add_equation("V_N = V_N[-1] + ( YD_N - C_N )") |>
      add_equation("V_S = V_S[-1] + ( YD_S - C_S )") |>
      add_equation("C_N = alpha1_N * YD_N + alpha2_N * V_N[-1]") |>
      add_equation("C_S = alpha1_S * YD_S + alpha2_S * V_S[-1]") |>
      add_equation("Hh_N = V_N - Bh_N") |>
      add_equation("Hh_S = V_S - Bh_S") |>
      add_equation("Bh_N = V_N * ( lambda0_N + lambda1_N * r - lambda2_N * ( YD_N/V_N ) )") |>
      add_equation("Bh_S = V_S * ( lambda0_S + lambda1_S * r - lambda2_S * ( YD_S/V_S ) )") |>
      add_equation("TX = TX_N + TX_S") |>
      add_equation("G = G_N + G_S") |>
      add_equation("Bh = Bh_N + Bh_S") |>
      add_equation("Hh = Hh_N + Hh_S") |>
      add_equation("Bs = Bs[-1] + ( G + r[-1] * Bs[-1] ) - ( TX + r[-1] * Bcb[-1] )") |>
      add_equation("Hs = Hs[-1] + Bcb - Bcb[-1]") |>
      add_equation("Bcb = Bs - Bh") |>
      add_equation("Hs = Hh", desc = "Money equilibrium", hidden = TRUE)
  } else if (model_template_name == "OPEN") {
    # parameters
    model <- model |>
      add_variable("xr", init = 1) |>
      add_variable("pg_N", init = 1) |>
      add_variable("r_N", init = 0.025) |>
      add_variable("r_S", init = 0.025) |>
      add_variable("G_S", init = 20) |>
      add_variable("G_N", init = 20) |>
      add_variable("mu_N", init = 0.15) |>
      add_variable("mu_S", init = 0.15) |>
      add_variable("alpha1_N", init = 0.7) |>
      add_variable("alpha1_S", init = 0.8) |>
      add_variable("alpha2_N", init = 0.3) |>
      add_variable("alpha2_S", init = 0.2) |>
      add_variable("lambda0_N", init = 0.67) |>
      add_variable("lambda0_S", init = 0.67) |>
      add_variable("lambda1_N", init = 0.05) |>
      add_variable("lambda1_S", init = 0.05) |>
      add_variable("lambda2_N", init = 0.01) |>
      add_variable("lambda2_S", init = 0.01) |>
      add_variable("theta_N", init = 0.2) |>
      add_variable("theta_S", init = 0.2) |>
      add_variable("Y_N") |>
      add_variable("Y_S") |>
      add_variable("C_N") |>
      add_variable("X_N") |>
      add_variable("IM_N") |>
      add_variable("C_S") |>
      add_variable("X_S") |>
      add_variable("IM_S") |>
      add_variable("YD_N") |>
      add_variable("YD_S") |>
      add_variable("TX_S") |>
      add_variable("TX_N") |>
      add_variable("Bh_S") |>
      add_variable("Bh_N") |>
      add_variable("V_N") |>
      add_variable("V_S") |>
      add_variable("Hh_N") |>
      add_variable("Hh_S") |>
      add_variable("Bs_N") |>
      add_variable("Bs_S") |>
      add_variable("Bcb_N") |>
      add_variable("Bcb_S") |>
      add_variable("or_N") |>
      add_variable("or_S") |>
      add_variable("Hs_N") |>
      add_variable("Hs_S") |>
      add_variable("pg_S") |>
      add_variable("deltaor_S") |>
      add_variable("deltaor_N")

    # equations
    model <- model |>
      add_equation("Y_N = C_N + G_N + X_N - IM_N") |>
      add_equation("Y_S = C_S + G_S + X_S - IM_S") |>
      add_equation("IM_N = mu_N * Y_N") |>
      add_equation("IM_S = mu_S * Y_S") |>
      add_equation("X_N = IM_S / xr") |>
      add_equation("X_S = IM_N * xr") |>
      add_equation("YD_N = Y_N - TX_N + r_N[-1] * Bh_N[-1]") |>
      add_equation("YD_S = Y_S - TX_S + r_S[-1] * Bh_S[-1]") |>
      add_equation("TX_N = theta_N * ( Y_N + r_N[-1] * Bh_N[-1])") |>
      add_equation("TX_S = theta_S * ( Y_S + r_S[-1] * Bh_S[-1])") |>
      add_equation("V_N = V_N[-1] + ( YD_N - C_N )") |>
      add_equation("V_S = V_S[-1] + ( YD_S - C_S )") |>
      add_equation("C_N = alpha1_N * YD_N + alpha2_N * V_N[-1]") |>
      add_equation("C_S = alpha1_S * YD_S + alpha2_S * V_S[-1]") |>
      add_equation("Hh_N = V_N - Bh_N") |>
      add_equation("Hh_S = V_S - Bh_S") |>
      add_equation("Bh_N = V_N * ( lambda0_N + lambda1_N * r_N - lambda2_N * ( YD_N/V_N ) )") |>
      add_equation("Bh_S = V_S * ( lambda0_S + lambda1_S * r_S - lambda2_S * ( YD_S/V_S ) )") |>
      add_equation("Bs_N = Bs_N[-1] + ( G_N + r_N[-1] * Bs_N[-1] ) - ( TX_N + r_N[-1] * Bcb_N[-1] )") |>
      add_equation("Bs_S = Bs_S[-1] + ( G_S + r_S[-1] * Bs_S[-1] ) - ( TX_S + r_S[-1] * Bcb_S[-1] )") |>
      add_equation("Bcb_N = Bs_N - Bh_N") |>
      add_equation("Bcb_S = Bs_S - Bh_S") |>
      add_equation("or_N = or_N[-1] + (( Hs_N - Hs_N[-1] - ( Bcb_N - Bcb_N[-1] ) )/pg_N)") |>
      add_equation("or_S = or_S[-1] + (( Hs_S - Hs_S[-1] - ( Bcb_S - Bcb_S[-1] ) )/pg_S)") |>
      add_equation("Hs_N = Hh_N") |>
      add_equation("Hs_S = Hh_S") |>
      add_equation("pg_S = pg_N * xr") |>
      add_equation("deltaor_S = or_S - or_S[-1]") |>
      add_equation("deltaor_N = - (or_N - or_N[-1])") |>
      add_equation("deltaor_S = deltaor_N", hidden = TRUE)
  } else if (model_template_name == "BMW") {
    model <- model |>
      add_variable("rl", init = 0.025) |>
      add_variable("alpha0", init = 20) |>
      add_variable("alpha1", init = 0.75) |>
      add_variable("alpha2", init = 0.10) |>
      add_variable("delta", init = 0.10) |>
      add_variable("gamma", init = 0.15) |>
      add_variable("kappa", init = 1) |>
      add_variable("pr", init = 1) |>
      add_variable("Nd", init = .001) |>
      add_variable("Ns", init = .001) |>
      add_variable("Y", init = .001) |>
      add_variable("Cs") |>
      add_variable("Cd") |>
      add_variable("Is") |>
      add_variable("Id") |>
      add_variable("Ls") |>
      add_variable("Ld") |>
      add_variable("WBd") |>
      add_variable("AF") |>
      add_variable("K") |>
      add_variable("YD") |>
      add_variable("WBs") |>
      add_variable("rm") |>
      add_variable("Mh") |>
      add_variable("Ms") |>
      add_variable("W") |>
      add_variable("DA") |>
      add_variable("KT")

    # equations
    model <- model |>
      add_equation("Cs = Cd") |>
      add_equation("Is = Id") |>
      add_equation("Ns = Nd") |>
      add_equation("Ls = Ls[-1] + Ld - Ld[-1]") |>
      add_equation("Y = Cs + Is") |>
      add_equation("WBd = Y - rl[-1] * Ld[-1] - AF") |>
      add_equation("AF = delta * K[-1]") |>
      add_equation("Ld = Ld[-1] + Id - AF") |>
      add_equation("YD = WBs + rm[-1] * Mh[-1]") |>
      add_equation("Mh = Mh[-1] + YD - Cd") |>
      add_equation("Ms = Ms[-1] + Ls - Ls[-1]") |>
      add_equation("rm = rl") |>
      add_equation("WBs = W * Ns") |>
      add_equation("Nd = Y / pr") |>
      add_equation("W = WBd / Nd") |>
      add_equation("Cd = alpha0 + alpha1 * YD + alpha2 * Mh[-1]") |>
      add_equation("K = K[-1] + Id - DA") |>
      add_equation("DA = delta * K[-1]") |>
      add_equation("KT = kappa * Y[-1]") |>
      add_equation("Id = gamma * (KT - K[-1]) + DA") |>
      add_equation("Ms = Mh", hidden = T)
  } else if (model_template_name == "BMWK") {
    model <- model |>
      # parameters
      add_variable("rl", init = 0.025) |>
      add_variable("alpha0", init = 20) |>
      add_variable("alpha2", init = 0.10) |>
      add_variable("delta", init = 0.10) |>
      add_variable("gamma", init = 0.15) |>
      add_variable("kappa", init = 1) |>
      add_variable("pr", init = 1) |>
      add_variable("Nd", init = .001) |>
      add_variable("Ns", init = .001) |>
      add_variable("Y", init = .001) |>
      add_variable("alpha1w", init = .8) |>
      add_variable("alpha1r", init = .15) |>
      add_variable("Cs") |>
      add_variable("Cd") |>
      add_variable("Is") |>
      add_variable("Id") |>
      add_variable("Ls") |>
      add_variable("Ld") |>
      add_variable("WBd") |>
      add_variable("AF") |>
      add_variable("K") |>
      add_variable("YD") |>
      add_variable("WBs") |>
      add_variable("rm") |>
      add_variable("Mh") |>
      add_variable("Ms") |>
      add_variable("W") |>
      add_variable("DA") |>
      add_variable("KT")

    # equations
    model <- model |>
      add_equation("Cs = Cd") |>
      add_equation("Is = Id") |>
      add_equation("Ns = Nd") |>
      add_equation("Ls = Ls[-1] + Ld - Ld[-1]") |>
      add_equation("Y = Cs + Is") |>
      add_equation("WBd = Y - rl[-1] * Ld[-1] - AF") |>
      add_equation("AF = delta * K[-1]") |>
      add_equation("Ld = Ld[-1] + Id - AF") |>
      add_equation("YD = WBs + rm[-1] * Mh[-1]") |>
      add_equation("Mh = Mh[-1] + YD - Cd") |>
      add_equation("Ms = Ms[-1] + Ls - Ls[-1]") |>
      add_equation("rm = rl") |>
      add_equation("WBs = W * Ns") |>
      add_equation("Nd = Y / pr") |>
      add_equation("W = WBd / Nd") |>
      add_equation("Cd = alpha0 + alpha1w * WBs + alpha1r * rm[-1] * Mh[-1] + alpha2 * Mh") |>
      add_equation("K = K[-1] + Id - DA") |>
      add_equation("DA = delta * K[-1]") |>
      add_equation("KT = kappa * Y[-1]") |>
      add_equation("Id = gamma * (KT - K[-1]) + DA") |>
      add_equation("Ms = Mh", hidden = T)
  } else if (model_template_name == "DIS") {
    # parameteres
    model <- model |>
      add_variable("rl", init = 0.025) |>
      add_variable("pr", init = 1) |>
      add_variable("W", init = 0.75) |>
      add_variable("add", init = 0.02) |>
      add_variable("alpha0", init = 15) |>
      add_variable("alpha1", init = 0.8) |>
      add_variable("alpha2", init = 0.1) |>
      add_variable("beta", init = 0.75) |>
      add_variable("epsilon", init = 0.75) |>
      add_variable("gamma", init = 0.25) |>
      add_variable("phi", init = 0.25) |>
      add_variable("sigma_T", init = 0.15) |>
      add_variable("y", init = .001) |>
      add_variable("p", init = .001) |>
      add_variable("NHUC", init = .001) |>
      add_variable("s_E", init = .001) |>
      add_variable("inv_T") |>
      add_variable("inv_E") |>
      add_variable("inv") |>
      add_variable("s") |>
      add_variable("c") |>
      add_variable("N") |>
      add_variable("WB") |>
      add_variable("UC") |>
      add_variable("INV") |>
      add_variable("S") |>
      add_variable("EF") |>
      add_variable("Ld") |>
      add_variable("Ls") |>
      add_variable("Ms") |>
      add_variable("rm") |>
      add_variable("EFb") |>
      add_variable("Mh") |>
      add_variable("YD") |>
      add_variable("C") |>
      add_variable("ydhs") |>
      add_variable("mh") |>
      add_variable("ydhs_E")

    # equations
    model <- model |>
      add_equation("y = s_E + inv_E - inv[-1]") |>
      add_equation("inv_T = sigma_T * s_E") |>
      add_equation("inv_E = inv[-1] + gamma * (inv_T - inv[-1])") |>
      add_equation("inv = inv[-1] + (y - s)") |>
      add_equation("s_E = beta * s[-1] + (1 - beta) * s_E[-1]") |>
      add_equation("s = c") |>
      add_equation("N = y / pr") |>
      add_equation("WB = N * W") |>
      add_equation("UC = WB / y") |>
      add_equation("INV = inv * UC") |>
      add_equation("S = p * s") |>
      add_equation("p = (1 + phi) * NHUC") |>
      add_equation("NHUC = (1 - sigma_T) * UC + sigma_T * (1 + rl[-1]) * UC[-1]") |>
      add_equation("EF = S - WB + (INV - INV[-1]) - rl[-1] * INV[-1]") |>
      add_equation("Ld = INV") |>
      add_equation("Ls = Ld") |>
      add_equation("Ms = Ls") |>
      add_equation("rm = rl - add") |>
      add_equation("EFb = rl[-1] * Ls[-1] - rm[-1] * Mh[-1]") |>
      add_equation("YD = WB + EF + EFb + rm[-1] * Mh[-1]") |>
      add_equation("Mh = Mh[-1] + YD - C") |>
      add_equation("ydhs = c + (mh - mh[-1])") |>
      add_equation("C = c * p") |>
      add_equation("mh = Mh / p") |>
      add_equation("c = alpha0 + alpha1 * ydhs_E + alpha2 * mh[-1]") |>
      add_equation("ydhs_E = epsilon * ydhs[-1] + (1 - epsilon) * ydhs_E[-1]") |>
      add_equation("Mh = Ms", hidden = T)
  } else if (model_template_name == "DISINF") {
    # variables
    model <- model |>
      add_variable("rrc", init = 0.025) |>
      add_variable("pr", init = 1) |>
      add_variable("add", init = 0.02) |>
      add_variable("alpha0", init = 15) |>
      add_variable("alpha1", init = 0.8) |>
      add_variable("alpha2", init = 0.1) |>
      add_variable("beta", init = 0.9) |>
      add_variable("epsilon", init = 0.8) |>
      add_variable("gamma", init = 0.25) |>
      add_variable("phi", init = 0.24) |>
      add_variable("sigma_T", init = 0.2) |>
      add_variable("Omega0", init = -1.4) |>
      add_variable("Omega1", init = 1) |>
      add_variable("Omega2", init = 1.2) |>
      add_variable("Omega3", init = 0.3) |>
      add_variable("p", init = 1) |>
      add_variable("W", init = 1) |>
      add_variable("UC", init = 1) |>
      add_variable("s_E", init = .00001) |>
      add_variable("inv_T") |>
      add_variable("inv_E") |>
      add_variable("inv") |>
      add_variable("s") |>
      add_variable("c") |>
      add_variable("N") |>
      add_variable("WB") |>
      add_variable("INV") |>
      add_variable("S") |>
      add_variable("EF") |>
      add_variable("Ld") |>
      add_variable("Ls") |>
      add_variable("Ms") |>
      add_variable("rm") |>
      add_variable("EFb") |>
      add_variable("Mh") |>
      add_variable("YD") |>
      add_variable("C") |>
      add_variable("omega_T") |>
      add_variable("Nfe") |>
      add_variable("yfe") |>
      add_variable("mh") |>
      add_variable("y") |>
      add_variable("rl") |>
      add_variable("pic") |>
      add_variable("ydhs") |>
      add_variable("yd") |>
      add_variable("ydhs_E")

    # equations
    model <- model |>
      add_equation("y = s_E + inv_E - inv[-1]") |>
      add_equation("inv_T = sigma_T * s_E") |>
      add_equation("inv_E = inv[-1] + gamma * (inv_T - inv[-1])") |>
      add_equation("inv = inv[-1] + (y - s)") |>
      add_equation("s_E = beta * s[-1] + (1 - beta) * s_E[-1]") |>
      add_equation("s = c") |>
      add_equation("N = y / pr") |>
      add_equation("WB = N * W") |>
      add_equation("UC = WB / y") |>
      add_equation("INV = inv * UC") |>
      add_equation("S = p * s") |>
      add_equation("p = (1 + phi) * (1 + rrc * sigma_T) * UC") |>
      add_equation("EF = S - WB + (INV - INV[-1]) - rl * INV[-1]") |>
      add_equation("Ld = INV") |>
      add_equation("Ls = Ld") |>
      add_equation("Ms = Ls") |>
      add_equation("rm = rl - add") |>
      add_equation("EFb = rl[-1] * Ls[-1] - rm[-1] * Mh[-1]") |>
      add_equation("pic = (UC / UC[-1]) - 1") |>
      add_equation("rl = (1 + rrc) * (1 + pic) - 1") |>
      add_equation("YD = WB + EF + EFb + rm * Mh[-1]") |>
      add_equation("Mh = Mh[-1] + YD - C") |>
      add_equation("ydhs = c + (mh - mh[-1])") |>
      add_equation("yd = YD / p") |>
      add_equation("C = c * p") |>
      add_equation("mh = Mh / p") |>
      add_equation("c = alpha0 + alpha1 * ydhs_E + alpha2 * mh[-1]") |>
      add_equation("ydhs_E = epsilon * ydhs[-1] + (1 - epsilon) * ydhs_E[-1]") |>
      add_equation("omega_T = Omega0 + Omega1 * pr + Omega2 * (N / Nfe)") |>
      add_equation("W = W[-1] * (1 + Omega3 * (omega_T[-1] - (W[-1]/p[-1])))") |>
      add_equation("yfe = (1 + sigma_T) * s - inv[-1]") |>
      add_equation("Nfe = s / pr")
  } else if (model_template_name == "SIMEX") {
    model <- model |>
      # variables
      add_variable("C_d", desc = "Consumption demand by households") |>
      add_variable("C_s", desc = "Consumption supply") |>
      add_variable("G_s", desc = "Government supply") |>
      add_variable("T_d", desc = "Taxes, demand") |>
      add_variable("T_s", desc = "Taxes, supply") |>
      add_variable("N_d", desc = "Demand for labor") |>
      add_variable("N_s", desc = "Supply of labor") |>
      add_variable("H_h", desc = "Cash money held by households") |>
      add_variable("H_s", desc = "Cash money supplied by the government") |>
      add_variable("H_d", desc = "Cash money demanded by the government") |>
      add_variable("Y", desc = "Income = GDP") |>
      add_variable("Yd", desc = "Disposable income of households") |>
      add_variable("Yd_e", desc = "Expected disposable income of households") |>
      add_variable("alpha1", init = 0.6, desc = "Propensity to consume out of income") |>
      add_variable("alpha2", init = 0.4, desc = "Propensity to consume out of wealth") |>
      add_variable("theta", init = 0.2, desc = "Tax rate") |>
      add_variable("G_d", init = 20, desc = "Government demand") |>
      add_variable("W", init = 1, desc = "Wage rate")

    # equations
    model <- model |>
      add_equation("C_s = C_d", desc = "Consumption") |>
      add_equation("G_s = G_d") |>
      add_equation("T_s = T_d") |>
      add_equation("N_s = N_d") |>
      add_equation("Yd = W * N_s - T_s") |>
      add_equation("T_d = theta * W * N_s") |>
      add_equation("C_d = alpha1 * Yd_e + alpha2 * H_h[-1]") |>
      add_equation("H_s = G_d - T_d + H_s[-1]") |>
      add_equation("H_h = Yd - C_d + H_h[-1]") |>
      add_equation("Y = C_s + G_s") |>
      add_equation("N_d = Y/W") |>
      add_equation("H_d = Yd_e - C_d + H_h[-1]") |>
      add_equation("Yd_e = Yd[-1]") |>
      add_equation("H_s = H_h", desc = "Money equilibrium", hidden = TRUE)
  }
  else if (model_template_name == "INSOUT") {
    model <- model |>
      # variables
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
    
    # equations
    model <- model |>
      # Firm's behavioral equations
      add_equation("y = sE + (invE - inv[-1])", desc = "") |>
      add_equation("N = y / pr", desc = "") |>
      add_equation("WB = N * W", desc = "") |>
      add_equation("UC = WB / y", desc = "") |>
      add_equation("sE = beta * s[-1] + (1 - beta) * sE[-1]", desc = "") |>
      add_equation("invT = sigmaT * sE", desc = "") |>
      add_equation("sigmaT = sigma0 - sigma1 * rl", desc = "") |>
      add_equation("invE = inv[-1] + gamma * (invT - inv[-1])", desc = "") |>
      add_equation("p = (1 + tau) * (1 + phi) * NHUC", desc = "") |>
      add_equation("NHUC = (1 - sigmaT) * UC + sigmaT * (1 + rl[-1]) * UC[-1]", desc = "") |>
      
      # Firm's realized outcomes
      add_equation("s = c + g", desc = "") |>
      add_equation("S = s * p", desc = "") |>
      add_equation("inv = inv[-1] + y - s", desc = "") |>
      add_equation("sigmas = inv[-1] / s", desc = "") |>
      add_equation("INV = inv * UC", desc = "") |>
      add_equation("Ld = INV", desc = "") |>
      add_equation("FXf = S - TX - WB + (INV - INV[-1]) - rl[-1] * INV[-1]", desc = "") |>
      add_equation("pi = (p / p[-1]) - 1", desc = "") |>
      
      # Households realized outcomes
      add_equation("YDr = FX + WB + rm[-1] * M2h[-1] + rb[-1] * Bhh[-1] + BLh[-1]", desc = "") |>
      add_equation("CG = (pbl - pbl[-1]) * BLh[-1]", desc = "") |>
      add_equation("YDhs = YDr + CG", desc = "") |>
      add_equation("FX = FXf + FXb", desc = "") |>
      add_equation("V = V[-1] + YDhs - C", desc = "") |>
      add_equation("Vnc = V - Hhh", desc = "") |>
      add_equation("ydr = YDr/p - pi * (V[-1]/p)", desc = "") |>
      add_equation("ydhs = (YDr - pi * V[-1] + CG) / p", desc = "") |>
      add_equation("v = V/p", desc = "") |>
      
      # Households behavioral
      add_equation("c = alpha0 + alpha1 * ydrE + alpha2 * v[-1]", desc = "") |>
      add_equation("ydrE = epsilon * ydr[-1] + (1 - epsilon) * ydrE[-1]", desc = "") |>
      add_equation("C = p * c", desc = "") |>
      add_equation("YDrE = p * ydrE + pi * (V[-1]/p)", desc = "") |>
      add_equation("VE = V[-1] + (YDrE - C)", desc = "") |>
      add_equation("Hhd = lambdac * C", desc = "") |>
      add_equation("VncE = VE - Hhd", desc = "") |>
      add_equation("ERrbl = rbl", desc = "") |>
      
      # Households' portfolio equations
      add_equation("M2d = VncE * (lambda20 + lambda22 * rm + lambda23 * rb + lambda24 * ERrbl + lambda25 * (YDrE / VncE))", desc = "") |>
      add_equation("Bhd = VncE * (lambda30 + lambda32 * rm + lambda33 * rb + lambda34 * ERrbl + lambda35 * (YDrE / VncE))", desc = "") |>
      add_equation("BLd = (VncE / pbl) * (lambda40 + lambda42 * rm + lambda43 * rb + lambda44 * ERrbl + lambda45 * (YDrE / VncE))", desc = "") |>
      add_equation("M1d = VncE * (lambda10 + lambda12 * rm + lambda13 * rb + lambda14 * ERrbl + lambda15 * (YDrE / VncE))", desc = "") |>
      add_equation("M1d2 = VncE - M2d - Bhd - pbl * BLd", desc = "") |>
      
      # Realized portfolio asset holdings
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
      add_equation("GD = GD[-1] + PSBR", desc = "") |>
      
      # Central bank's equations
      add_equation("Hs = Bcb + As", desc = "") |>
      add_equation("Hbs = Hs - Hhs", desc = "") |>
      add_equation("Bcb = Bs - Bhh - Bbd", desc = "") |>
      add_equation("As = Ad", desc = "") |>
      add_equation("ra = rb", desc = "") |>
      add_equation("FXcb = rb[-1] * Bcb[-1] + ra[-1] * As[-1]", desc = "") |>
      
      # Bank's realized (supply) equations
      add_equation("Hhs = Hhd", desc = "") |>
      add_equation("M1s = M1h", desc = "") |>
      add_equation("M2s = M2h", desc = "") |>
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
      add_equation("lM1s = M1s[-1]", desc = "") |>
      add_equation("lM2s = M2s[-1]", desc = "") |>
      add_equation("BPM = (FXb + FXb[-1]) / (lM1s + lM1s[-1] + lM2s + lM2s[-1])", desc = "") |>
      # Inflationary forces
      add_equation("omegaT = exp(Omega0 + Omega1 * log(pr) + Omega2 * log(N / Nfe))", desc = "") |>
      add_equation("W = W[-1] * (1 + Omega3 * (omegaT[-1] - (W[-1] / p[-1])))", desc = "") |>
      add_equation("Y = p * s + UC * (inv - inv[-1])", desc = "") |>
      
      # Hidden equation
      add_equation("Hbd = Hbs", hidden = TRUE)
  }
  else if (model_template_name == "GROWTH") {
    model <- model |>
      # variables
      add_variable("alpha1", init = 0.75) |>
      add_variable("alpha2", init = 0.064) |>
      add_variable("beta", init = 0.5) |>
      add_variable("betab", init = 0.4) |>
      add_variable("gamma", init = 0.15) |>
      add_variable("gamma0", init = 0.00122) |>
      add_variable("gammar", init = 0.1) |>
      add_variable("gammau", init = 0.05) |>
      add_variable("delta", init = 0.10667) |>
      add_variable("deltarep", init = 0.1) |>
      add_variable("eps", init = 0.5) |>
      add_variable("eps2", init = 0.8) |>
      add_variable("epsb", init = 0.25) |>
      add_variable("epsrb", init = 0.9) |>
      add_variable("eta0", init = 0.07416) |>
      add_variable("etan", init = 0.6) |>
      add_variable("etar", init = 0.4) |>
      add_variable("theta", init = 0.22844) |>
      add_variable("lambda20", init = 0.25) |>
      add_variable("lambda21", init = 2.2) |>
      add_variable("lambda22", init = 6.6) |>
      add_variable("lambda23", init = 2.2) |>
      add_variable("lambda24", init = 2.2) |>
      add_variable("lambda25", init = 0.1) |>
      add_variable("lambda30", init = -0.04341) |>
      add_variable("lambda31", init = 2.2) |>
      add_variable("lambda32", init = 2.2) |>
      add_variable("lambda33", init = 6.6) |>
      add_variable("lambda34", init = 2.2) |>
      add_variable("lambda35", init = 0.1) |>
      add_variable("lambda40", init = 0.67132) |>
      add_variable("lambda41", init = 2.2) |>
      add_variable("lambda42", init = 2.2) |>
      add_variable("lambda43", init = 2.2) |>
      add_variable("lambda44", init = 6.6) |>
      add_variable("lambda45", init = 0.1) |>
      add_variable("lambdab", init = 0.0153) |>
      add_variable("lambdac", init = 0.05) |>
      add_variable("xim1", init = 0.0008) |>
      add_variable("xim2", init = 0.0007) |>
      add_variable("ro", init = 0.05) |>
      add_variable("sigman", init = 0.1666) |>
      add_variable("sigmat", init = 0.2) |>
      add_variable("psid", init = 0.15255) |>
      add_variable("psiu", init = 0.92) |>
      add_variable("omega0", init = -0.20594) |>
      add_variable("omega1", init = 1) |>
      add_variable("omega2", init = 2) |>
      add_variable("omega3", init = 0.45621) |>
      add_variable("ADDbl", init = 0.02) |>
      add_variable("BANDt", init = 0.01) |>
      add_variable("BANDb", init = 0.01) |>
      add_variable("bot", init = 0.05) |>
      add_variable("GRg", init = 0.03) |>
      add_variable("GRpr", init = 0.03) |>
      add_variable("Nfe", init = 87.181) |>
      add_variable("NCAR", init = 0.1) |>
      add_variable("NPLk", init = 0.02) |>
      add_variable("Rbbar", init = 0.035) |>
      add_variable("Rln", init = 0.07) |>
      add_variable("RA", init = 0) |>
      add_variable("top", init = 0.12) |>
      add_variable("sigmase", init = 0.16667) |>
      add_variable("eta", init = 0.04918) |>
      add_variable("phi", init = 0.26417) |>
      add_variable("phit", init = 0.26417) |>
      add_variable("ADDl", init = 0.04592) |>
      add_variable("BLR", init = 0.1091) |>
      add_variable("BUR", init = 0.06324) |>
      add_variable("Ck", init = 7334240) |>
      add_variable("CAR", init = 0.09245) |>
      add_variable("CONS", init = 52603100) |>
      add_variable("ER", init = 1) |>
      add_variable("Fb", init = 1744130) |>
      add_variable("Fbt", init = 1744140) |>
      add_variable("Ff", init = 18081100) |>
      add_variable("Fft", init = 18013600) |>
      add_variable("FDb", init = 1325090) |>
      add_variable("FDf", init = 2670970) |>
      add_variable("FUb", init = 419039) |>
      add_variable("FUf", init = 15153800) |>
      add_variable("FUft", init = 15066200) |>
      add_variable("G", init = 16755600) |>
      add_variable("Gk", init = 2336160) |>
      add_variable("GL", init = 2775900) |>
      add_variable("GRk", init = 0.03001) |>
      add_variable("INV", init = 16911600) |>
      add_variable("Ik", init = 2357910) |>
      add_variable("N", init = 87.181) |>
      add_variable("Nt", init = 87.181) |>
      add_variable("NHUC", init = 5.6735) |>
      add_variable("NL", init = 683593) |>
      add_variable("NLk", init = 95311) |>
      add_variable("NPL", init = 309158) |>
      add_variable("NPLke", init = 0.02) |>
      add_variable("NUC", init = 5.6106) |>
      add_variable("omegat", init = 112852) |>
      add_variable("P", init = 7.1723) |>
      add_variable("Pbl", init = 18.182) |>
      add_variable("Pe", init = 17937) |>
      add_variable("PE", init = 5.07185) |>
      add_variable("PI", init = 0.0026) |>
      add_variable("PR", init = 138659) |>
      add_variable("PSBR", init = 1894780) |>
      add_variable("Q", init = 0.77443) |>
      add_variable("Rb", init = 0.035) |>
      add_variable("Rbl", init = 0.055) |>
      add_variable("Rk", init = 0.03008) |>
      add_variable("Rl", init = 0.06522) |>
      add_variable("Rm", init = 0.0193) |>
      add_variable("REP", init = 2092310) |>
      add_variable("RRl", init = 0.06246) |>
      add_variable("S", init = 86270300) |>
      add_variable("Sk", init = 12028300) |>
      add_variable("Ske", init = 12028300) |>
      add_variable("TX", init = 17024100) |>
      add_variable("U", init = 0.70073) |>
      add_variable("UC", init = 5.6106) |>
      add_variable("W", init = 777968) |>
      add_variable("WB", init = 67824000) |>
      add_variable("Y", init = 86607700) |>  
      add_variable("Yk", init = 12088400) |>
      add_variable("YDr", init = 56446400) |>
      add_variable("YDkr", init = 7813270) |>
      add_variable("YDkre", init = 7813290) |>
      add_variable("YP", init = 73158700) |>
      add_variable("z1a", init = 0) |>
      add_variable("z1b", init = 0) |>
      add_variable("z2a", init = 0) |>  
      add_variable("z2b", init = 0) |>
      add_variable("Bbd", init = 4389790) |>
      add_variable("Bbs", init = 4389790) |>
      add_variable("Bcbd", init = 4655690) |>
      add_variable("Bcbs", init = 4655690) |>
      add_variable("Bhd", init = 33439320) |>
      add_variable("Bhs", init = 33439320) |>
      add_variable("Bs", init = 42484800) |>
      add_variable("BLd", init = 840742) |>
      add_variable("BLs", init = 840742) |>
      add_variable("GD", init = 57728700) |>
      add_variable("Ekd", init = 5112.6001) |>
      add_variable("Eks", init = 5112.6001) |>
      add_variable("Hbd", init = 2025540) |>
      add_variable("Hbs", init = 2025540) |>
      add_variable("Hhd", init = 2630150) |>
      add_variable("Hhs", init = 2630150) |>
      add_variable("Hs", init = 4655690) |>
      add_variable("IN", init = 11585400) |>
      add_variable("INk", init = 2064890) |>
      add_variable("INke", init = 2405660) |>
      add_variable("INkt", init = 2064890) |>
      add_variable("K", init = 127486471) |>
      add_variable("Kk", init = 17774838) |>
      add_variable("Lfd", init = 15962900) |>
      add_variable("Lfs", init = 15962900) |>
      add_variable("Lhd", init = 21606600) |>
      add_variable("Lhs", init = 21606600) |>
      add_variable("Ls", init = 37569500) |>
      add_variable("Mh", init = 40510800) |>
      add_variable("Ms", init = 40510800) |>
      add_variable("OFb", init = 3474030) |>
      add_variable("OFbe", init = 3474030) |>
      add_variable("OFbt", init = 3638100) |>
      add_variable("V", init = 165438779) |>
      add_variable("Vfma", init = 159334599) |>
      add_variable("Vk", init = 23066350) |>
      add_variable("Vf", init = 31361792) |>
      add_variable("z3") |>
      add_variable("z4") |>
      add_variable("z5") |>
      add_variable("z3a") |>
      add_variable("z3b") |>
      add_variable("HCe") |>
      add_variable("YDhs") |>
      add_variable("CG") |>
      add_variable("VfmaA") |>
      add_variable("Fcb") |>
      add_variable("FUbt")
    
    # equations
    model <- model |>
      add_equation("Yk = Ske + INke - INk[-1]", desc = "11.1 : Real output") |>
      add_equation("Ske = beta*Sk + (1-beta)*Sk[-1]*(1 + (GRpr + RA))", desc = "11.2 : Expected real sales") |>
      add_equation("INke = INk[-1] + gamma*(INkt - INk[-1])", desc = "11.3 : Long-run inventory target") |>
      add_equation("INkt = sigmat*Ske", desc = "11.4 : Short-run inventory target") |>
      add_equation("INk = INk[-1] + Yk - Sk - NPL/UC", desc = "11.5 : Actual real inventories") |>
      add_equation("Kk = Kk[-1]*(1 + GRk)", desc = "11.6 : Real capital stock") |>
      add_equation("GRk = gamma0 + gammau*U[-1] - gammar*RRl", desc = "11.7 : Growth of real capital stock") |>
      add_equation("U = Yk/Kk[-1]", desc = "11.8 : Capital utilization proxy") |>
      add_equation("RRl = ((1 + Rl)/(1 + PI)) - 1", desc = "11.9 : Real interest rate on loans") |>
      add_equation("PI = (P - P[-1])/P[-1]", desc = "11.10 : Rate of price inflation") |>
      add_equation("Ik = (Kk - Kk[-1]) + delta*Kk[-1]", desc = "11.11 : Real gross investment") |>
      
      # Box 11.2 : Firms equations
      # ---------------------------
      add_equation("Sk = Ck + Gk + Ik", desc = "11.12 : Actual real sales") |>
      add_equation("S = Sk*P", desc = "11.13 : Value of realized sales") |>
      add_equation("IN = INk*UC", desc = "11.14 : Inventories valued at current cost") |>
      add_equation("INV = Ik*P", desc = "11.15 : Nominal gross investment") |>
      add_equation("K = Kk*P", desc = "11.16 : Nomincal value of fixed capital") |>
      add_equation("Y = Sk*P + (INk - INk[-1])*UC", desc = "11.17 : Nomincal GDP") |>
      
      # Box 11.3 : Firms equations
      # ---------------------------
      # 11.18 : Real wage aspirations
      add_equation("omegat = exp(omega0 + omega1*log(PR) + omega2*log(ER + z3*(1 - ER) - z4*BANDt + z5*BANDb))", desc = "") |>
      add_equation("ER = N[-1]/Nfe[-1]", desc = "11.19 : Employment rate") |>
      # 11.20 : Switch variables
      add_equation("z3a = (ER > (1 - BANDb))", desc = "") |>
      add_equation("z3b = 1 - as.numeric(ER > (1 + BANDt))", desc = "") |>
      add_equation("z3 =  z3a * z3b", desc = "") |>
      add_equation("z4  = (ER >  (1 + BANDt))", desc = "") |>
      add_equation("z5  = (ER <  (1 - BANDb))", desc = "") |>
      add_equation("W = W[-1] + omega3*(omegat*P[-1] - W[-1])", desc = "11.21 : Nominal wage") |>
      add_equation("PR = PR[-1]*(1 + GRpr)", desc = "11.22 : Labor productivity") |>
      add_equation("Nt = Yk/PR", desc = "11.23 : Desired employment") |>
      add_equation("N = N[-1] + etan*(Nt - N[-1])", desc = "11.24 : Actual employment --> etan not in the book") |>
      add_equation("WB = N*W", desc = "11.25 : Nominal wage bill") |>
      add_equation("UC = WB/Yk", desc = "11.26 : Actual unit cost") |>
      add_equation("NUC = W/PR", desc = "11.27 : Normal unit cost") |>
      add_equation("NHUC = (1 - sigman)*NUC + sigman*(1 + Rln[-1])*NUC[-1]", desc = "11.28 : Normal historic unit cost") |>
      
      # Box 11.4 : Firms equations
      # ---------------------------
      add_equation("P = (1 + phi)*NHUC", desc = "11.29 : Normal-cost pricing") |>
      add_equation("phi = phi[-1] + eps2*(phit[-1] - phi[-1])", desc = "11.30 : Actual mark-up --> eps2 not in the book") |>
      # 11.31 : Ideal mark-up
      add_equation("phit = (FUft + FDf + Rl[-1]*(Lfd[-1] - IN[-1])) / ((1 - sigmase)*Ske*UC + (1 + Rl[-1])*sigmase*Ske*UC[-1])", desc = "") |>
      add_equation("HCe = (1 - sigmase)*Ske*UC + (1 + Rl[-1])*sigmase*Ske*UC[-1]", desc = "11.32 : Expected historical costs") |>
      add_equation("sigmase = INk[-1]/Ske", desc = "11.33 : Opening inventories to expected sales ratio") |>
      add_equation("Fft = FUft + FDf + Rl[-1]*(Lfd[-1] - IN[-1])", desc = "11.34 : Planned entrepeneurial profits of firms") |>
      add_equation("FUft = psiu*INV[-1]", desc = "11.35 : Planned retained earnings of firms") |>
      add_equation("FDf = psid*Ff[-1]", desc = "11.36 : Dividends of firms") |>
      
      # Box 11.5 : Firms equations
      # ---------------------------
      add_equation("Ff = S - WB + (IN - IN[-1]) - Rl[-1]*IN[-1]", desc = "11.37 : Realized entrepeneurial profits") |>
      add_equation("FUf = Ff - FDf - Rl[-1]*(Lfd[-1] - IN[-1]) + Rl[-1]*NPL", desc = "11.38 : Retained earnings of firms") |>
      # 11.39 : Demand for loans by firms
      add_equation("Lfd = Lfd[-1] + INV + (IN - IN[-1]) - FUf - (Eks - Eks[-1])*Pe - NPL", desc = "") |>
      add_equation("NPL = NPLk * Lfs[-1]", desc = "11.40 : Defaulted loans") |>
      add_equation("Eks = Eks[-1] + ((1 - psiu)*INV[-1])/Pe", desc = "11.41 : Supply of equities issued by firms") |>
      add_equation("Rk = FDf/(Pe[-1]*Ekd[-1])", desc = "11.42 : Dividend yield of firms") |>
      add_equation("PE = Pe/(Ff/Eks[-1])", desc = "11.43 : Price earnings ratio") |>
      add_equation("Q = (Eks*Pe + Lfd)/(K + IN)", desc = "11.44 : Tobins Q ratio") |>
      
      # Box 11.6 : Households equations
      # --------------------------------
      add_equation("YP = WB + FDf + FDb + Rm[-1]*Mh[-1] + Rb[-1]*Bhd[-1] + BLs[-1]", desc = "") |>
      add_equation("TX = theta*YP", desc = "11.46 : Income taxes") |>
      add_equation("YDr = YP - TX - Rl[-1]*Lhd[-1]", desc = "11.47 : Regular disposable income") |>
      add_equation("YDhs = YDr + CG", desc = "11.48 : Haig-Simons disposable income") |>
      # 11.49 : Capital gains
      add_equation("CG = (Pbl - Pbl[-1])*BLd[-1] + (Pe - Pe[-1])*Ekd[-1] + (OFb - OFb[-1])", desc = "") |>
      # 11.50 : Wealth
      add_equation("V = V[-1] + YDr - CONS + (Pbl - Pbl[-1])*BLd[-1] + (Pe - Pe[-1])*Ekd[-1] + (OFb - OFb[-1])", desc = "") |>
      add_equation("Vk = V/P", desc = "11.51 : Real stock of wealth") |>
      add_equation("CONS = Ck*P", desc = "11.52 : Consumption") |>
      add_equation("Ck = alpha1*(YDkre + NLk) + alpha2*Vk[-1]", desc = "11.53 : Real consumption") |>
      add_equation("YDkre = eps*YDkr + (1 - eps)*(YDkr[-1]*(1 + GRpr))", desc = "11.54 : Expected real regular disposable income") |>
      add_equation("YDkr = YDr/P - ((P - P[-1]) * Vk[-1])/P", desc = "11.55 : Real regular disposable income") |>
      
      # Box 11.7 : Households equations
      # --------------------------------
      add_equation("GL = eta*YDr", desc = "11.56 : Gross amount of new personal loans ---> new eta here") |>
      add_equation("eta = eta0 - etar*RRl", desc = "11.57 : New loans to personal income ratio") |>
      add_equation("NL = GL - REP", desc = "11.58 : Net amount of new personal loans") |>
      add_equation("REP = deltarep*Lhd[-1]", desc = "11.59 : Personal loans repayments") |>
      add_equation("Lhd = Lhd[-1] + GL - REP", desc = "11.60 : Demand for personal loans") |>
      add_equation("NLk = NL/P", desc = "11.61 : Real amount of new personal loans") |>
      add_equation("BUR = (REP + Rl[-1] * Lhd[-1]) / YDr[-1]", desc = "11.62 : Burden of personal debt") |>
      
      # Box 11.8 : Households equations - portfolio decisions
      # -----------------------------------------------------
      # 11.64 : Demand for bills
      add_equation("Bhd = Vfma[-1]*(lambda20 + lambda22*Rb[-1] - lambda21*Rm[-1] - lambda24*Rk[-1] - lambda23*Rbl[-1] - lambda25*(YDr/V))", desc = "") |>
      # 11.65 : Demand for bonds
      add_equation("BLd = Vfma[-1]*(lambda30 - lambda32*Rb[-1] - lambda31*Rm[-1] - lambda34*Rk[-1] + lambda33*Rbl[-1] - lambda35*(YDr/V))/Pbl", desc = "") |>
      # 11.66 : Demand for equities - normalized to get the price of equitities
      add_equation("Pe = Vfma[-1]*(lambda40 - lambda42*Rb[-1] - lambda41*Rm[-1] + lambda44*Rk[-1] - lambda43*Rbl[-1] - lambda45*(YDr/V))/Ekd", desc = "") |>
      add_equation("Mh = Vfma - Bhd - Pe*Ekd - Pbl*BLd + Lhd", desc = "11.67 : Money deposits - as a residual") |>
      add_equation("Vfma = V - Hhd - OFb", desc = "11.68 : Investible wealth") |>
      add_equation("VfmaA = Mh + Bhd + Pbl * BLd + Pe * Ekd", desc = "") |>
      add_equation("Hhd = lambdac*CONS", desc = "11.69 : Households demand for cash") |>
      add_equation("Ekd = Eks", desc = "11.70 : Stock market equilibrium") |>
      
      # Box 11.9 : Governments equations
      # ---------------------------------
      add_equation("G = Gk*P", desc = "11.71 : Pure government expenditures") |>
      add_equation("Gk = Gk[-1]*(1 + GRg)", desc = "11.72 : Real government expenditures") |>
      add_equation("PSBR = G + BLs[-1] + Rb[-1]*(Bbs[-1] + Bhs[-1]) - TX", desc = "11.73 : Government deficit --> BLs[-1] missing in the book") |>
      # 11.74 : New issues of bills
      add_equation("Bs = Bs[-1] + G - TX - (BLs - BLs[-1])*Pbl + Rb[-1]*(Bhs[-1] + Bbs[-1]) + BLs[-1]", desc = "") |>
      add_equation("GD = Bbs + Bhs + BLs*Pbl + Hs", desc = "11.75 : Government debt") |>
      
      # Box 11.10 : The Central banks equations
      # ----------------------------------------
      add_equation("Fcb = Rb[-1]*Bcbd[-1]", desc = "11.76 : Central bank profits") |>
      add_equation("BLs = BLd", desc = "11.77 : Bonds are supplied on demand") |>
      add_equation("Bhs = Bhd", desc = "11.78 : Household bills supplied on demand") |>
      add_equation("Hhs = Hhd", desc = "11.79 : Cash supplied on demand --> Mistake on the book") |>
      add_equation("Hbs = Hbd", desc = "11.80 : Reserves supplied on demand") |>
      add_equation("Hs = Hbs + Hhs", desc = "11.81 : Total supply of cash") |>
      add_equation("Bcbd = Hs", desc = "11.82 : Central bankd ") |>
      add_equation("Bcbs = Bcbd", desc = "11.83 : Supply of bills to Central bank") |>
      add_equation("Rb = Rbbar", desc = "11.84 : Interest rate on bills set exogenously") |>
      add_equation("Rbl = Rb + ADDbl", desc = "11.85 : Long term interest rate") |>
      add_equation("Pbl = 1/Rbl", desc = "11.86 : Price of long-term bonds") |>
      
      # Box 11.11 : Commercial Banks equations
      # ---------------------------------------
      add_equation("Ms = Mh", desc = "11.87 : Bank deposits supplied on demand") |>
      add_equation("Lfs = Lfd", desc = "11.88 : Loans to firms supplied on demand") |>
      add_equation("Lhs = Lhd", desc = "11.89 : Personal loans supplied on demand") |>
      add_equation("Hbd = ro*Ms", desc = "11.90 Reserve requirements of banks") |>
      # 11.91 : Bills supplied to banks
      add_equation("Bbs = Bbs[-1] + (Bs - Bs[-1]) - (Bhs - Bhs[-1]) - (Bcbs - Bcbs[-1])", desc = "") |>
      # 11.92 : Balance sheet constraint of banks
      add_equation("Bbd = Ms + OFb - Lfs - Lhs - Hbd", desc = "") |>
      add_equation("BLR = Bbd/Ms", desc = "11.93 : Bank liquidity ratio") |>
      # 11.94 : Deposit interest rate
      add_equation("Rm = Rm[-1] + z1a*xim1 + z1b*xim2 - z2a*xim1 - z2b*xim2", desc = "") |>
      # 11.95-97 : Mechanism for determining changes to the interest rate on deposits
      add_equation("z2a = (BLR[-1] >  (top + 0.05))", desc = "") |>
      add_equation("z2b = (BLR[-1] >  top)", desc = "") |>
      add_equation("z1a = 1 - as.numeric(BLR[-1] > bot)", desc = "") |>
      add_equation("z1b = 1 - as.numeric(BLR[-1] > (bot - 0.05))", desc = "") |>
      
      # Box 11.12 : Commercial banks equations
      # ---------------------------------------
      add_equation("Rl = Rm + ADDl", desc = "11.98 : Loan interest rate") |>
      add_equation("OFbt = NCAR*(Lfs[-1] + Lhs[-1])", desc = "11.99 : Long-run own funds target") |>
      add_equation("OFbe = OFb[-1] + betab*(OFbt - OFb[-1])", desc = "11.100 : Short-run own funds target") |>
      add_equation("FUbt = OFbe - OFb[-1] + NPLke*Lfs[-1]", desc = "11.101 : Target retained earnings of banks") |>
      add_equation("NPLke = epsb*NPLke[-1] + (1 - epsb)*NPLk[-1]", desc = "11.102 : Expected proportion of non-performaing loans") |>
      add_equation("FDb = Fb - FUb", desc = "11.103 : Dividends of banks") |>
      add_equation("Fbt = lambdab*Y[-1] + (OFbe - OFb[-1] + NPLke*Lfs[-1])", desc = "11.104 : Target profits of banks") |>
      # 11.105 : Actual profits of banks
      add_equation("Fb = Rl[-1]*(Lfs[-1] + Lhs[-1] - NPL) + Rb[-1]*Bbd[-1] - Rm[-1]*Ms[-1]", desc = "") |>
      # 11.106 : Lending mark-up over deposit rate
      add_equation("ADDl = (Fbt - Rb[-1]*Bbd[-1] + Rm[-1]*(Ms[-1] - (1 - NPLke)*Lfs[-1] - Lhs[-1]))/((1 - NPLke)*Lfs[-1] + Lhs[-1])", desc = "--> I added the lag term to Rm") |>
      add_equation("FUb = Fb - lambdab*Y[-1]", desc = "11.107 : Actual retained earnings") |>
      add_equation("OFb = OFb[-1] + FUb - NPL", desc = "11.108 : Own funds of banks") |>
      add_equation("CAR = OFb/(Lfs + Lhs)", desc = "") |>
      add_equation("Vf = IN + K - Lfd - Ekd * Pe", desc = "Firm's wealth (memo for matrices)") |>
      add_equation("Ls = Lfs + Lhs", desc = "Loans supply (memo for matrices)") |>
      
      # Hidden equation
      # ---------------------------------------
      add_equation("Bbs = Bbd", desc = "", hidden = TRUE)
  }
  else if (model_template_name == "Advanced OPEN") {}
  
  return(model)
}
