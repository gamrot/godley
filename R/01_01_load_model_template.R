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
      
      # Firms equations
      add_equation("Sk = Ck + Gk + Ik", desc = "11.12 : Actual real sales") |>
      add_equation("S = Sk*P", desc = "11.13 : Value of realized sales") |>
      add_equation("IN = INk*UC", desc = "11.14 : Inventories valued at current cost") |>
      add_equation("INV = Ik*P", desc = "11.15 : Nominal gross investment") |>
      add_equation("K = Kk*P", desc = "11.16 : Nomincal value of fixed capital") |>
      add_equation("Y = Sk*P + (INk - INk[-1])*UC", desc = "11.17 : Nomincal GDP") |>
      add_equation("omegat = exp(omega0 + omega1*log(PR) + omega2*log(ER + z3*(1 - ER) - z4*BANDt + z5*BANDb))", desc = "11.18 : Real wage aspirations") |>
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
      add_equation("P = (1 + phi)*NHUC", desc = "11.29 : Normal-cost pricing") |>
      add_equation("phi = phi[-1] + eps2*(phit[-1] - phi[-1])", desc = "11.30 : Actual mark-up --> eps2 not in the book") |>
      add_equation("phit = (FUft + FDf + Rl[-1]*(Lfd[-1] - IN[-1])) / ((1 - sigmase)*Ske*UC + (1 + Rl[-1])*sigmase*Ske*UC[-1])", desc = "11.31 : Ideal mark-up") |>
      add_equation("HCe = (1 - sigmase)*Ske*UC + (1 + Rl[-1])*sigmase*Ske*UC[-1]", desc = "11.32 : Expected historical costs") |>
      add_equation("sigmase = INk[-1]/Ske", desc = "11.33 : Opening inventories to expected sales ratio") |>
      add_equation("Fft = FUft + FDf + Rl[-1]*(Lfd[-1] - IN[-1])", desc = "11.34 : Planned entrepeneurial profits of firms") |>
      add_equation("FUft = psiu*INV[-1]", desc = "11.35 : Planned retained earnings of firms") |>
      add_equation("FDf = psid*Ff[-1]", desc = "11.36 : Dividends of firms") |>
      add_equation("Ff = S - WB + (IN - IN[-1]) - Rl[-1]*IN[-1]", desc = "11.37 : Realized entrepeneurial profits") |>
      add_equation("FUf = Ff - FDf - Rl[-1]*(Lfd[-1] - IN[-1]) + Rl[-1]*NPL", desc = "11.38 : Retained earnings of firms") |>
      add_equation("Lfd = Lfd[-1] + INV + (IN - IN[-1]) - FUf - (Eks - Eks[-1])*Pe - NPL", desc = "11.39 : Demand for loans by firms") |>
      add_equation("NPL = NPLk * Lfs[-1]", desc = "11.40 : Defaulted loans") |>
      add_equation("Eks = Eks[-1] + ((1 - psiu)*INV[-1])/Pe", desc = "11.41 : Supply of equities issued by firms") |>
      add_equation("Rk = FDf/(Pe[-1]*Ekd[-1])", desc = "11.42 : Dividend yield of firms") |>
      add_equation("PE = Pe/(Ff/Eks[-1])", desc = "11.43 : Price earnings ratio") |>
      add_equation("Q = (Eks*Pe + Lfd)/(K + IN)", desc = "11.44 : Tobins Q ratio") |>
      
      # Households equations
      add_equation("YP = WB + FDf + FDb + Rm[-1]*Mh[-1] + Rb[-1]*Bhd[-1] + BLs[-1]", desc = "") |>
      add_equation("TX = theta*YP", desc = "11.46 : Income taxes") |>
      add_equation("YDr = YP - TX - Rl[-1]*Lhd[-1]", desc = "11.47 : Regular disposable income") |>
      add_equation("YDhs = YDr + CG", desc = "11.48 : Haig-Simons disposable income") |>
      add_equation("CG = (Pbl - Pbl[-1])*BLd[-1] + (Pe - Pe[-1])*Ekd[-1] + (OFb - OFb[-1])", desc = "11.49 : Capital gains") |>
      add_equation("V = V[-1] + YDr - CONS + (Pbl - Pbl[-1])*BLd[-1] + (Pe - Pe[-1])*Ekd[-1] + (OFb - OFb[-1])", desc = "11.50 : Wealth") |>
      add_equation("Vk = V/P", desc = "11.51 : Real stock of wealth") |>
      add_equation("CONS = Ck*P", desc = "11.52 : Consumption") |>
      add_equation("Ck = alpha1*(YDkre + NLk) + alpha2*Vk[-1]", desc = "11.53 : Real consumption") |>
      add_equation("YDkre = eps*YDkr + (1 - eps)*(YDkr[-1]*(1 + GRpr))", desc = "11.54 : Expected real regular disposable income") |>
      add_equation("YDkr = YDr/P - ((P - P[-1]) * Vk[-1])/P", desc = "11.55 : Real regular disposable income") |>
      add_equation("GL = eta*YDr", desc = "11.56 : Gross amount of new personal loans ---> new eta here") |>
      add_equation("eta = eta0 - etar*RRl", desc = "11.57 : New loans to personal income ratio") |>
      add_equation("NL = GL - REP", desc = "11.58 : Net amount of new personal loans") |>
      add_equation("REP = deltarep*Lhd[-1]", desc = "11.59 : Personal loans repayments") |>
      add_equation("Lhd = Lhd[-1] + GL - REP", desc = "11.60 : Demand for personal loans") |>
      add_equation("NLk = NL/P", desc = "11.61 : Real amount of new personal loans") |>
      add_equation("BUR = (REP + Rl[-1] * Lhd[-1]) / YDr[-1]", desc = "11.62 : Burden of personal debt") |>
      
      # Households equations - portfolio decisions
      add_equation("Bhd = Vfma[-1]*(lambda20 + lambda22*Rb[-1] - lambda21*Rm[-1] - lambda24*Rk[-1] - lambda23*Rbl[-1] - lambda25*(YDr/V))", desc = "11.64 : Demand for bills") |>
      add_equation("BLd = Vfma[-1]*(lambda30 - lambda32*Rb[-1] - lambda31*Rm[-1] - lambda34*Rk[-1] + lambda33*Rbl[-1] - lambda35*(YDr/V))/Pbl", desc = "11.65 : Demand for bonds") |>
      add_equation("Pe = Vfma[-1]*(lambda40 - lambda42*Rb[-1] - lambda41*Rm[-1] + lambda44*Rk[-1] - lambda43*Rbl[-1] - lambda45*(YDr/V))/Ekd", desc = "11.66 : Demand for equities - normalized to get the price of equitities") |>
      add_equation("Mh = Vfma - Bhd - Pe*Ekd - Pbl*BLd + Lhd", desc = "11.67 : Money deposits - as a residual") |>
      add_equation("Vfma = V - Hhd - OFb", desc = "11.68 : Investible wealth") |>
      add_equation("VfmaA = Mh + Bhd + Pbl * BLd + Pe * Ekd", desc = "") |>
      add_equation("Hhd = lambdac*CONS", desc = "11.69 : Households demand for cash") |>
      add_equation("Ekd = Eks", desc = "11.70 : Stock market equilibrium") |>
      
      # Governments equations
      add_equation("G = Gk*P", desc = "11.71 : Pure government expenditures") |>
      add_equation("Gk = Gk[-1]*(1 + GRg)", desc = "11.72 : Real government expenditures") |>
      add_equation("PSBR = G + BLs[-1] + Rb[-1]*(Bbs[-1] + Bhs[-1]) - TX", desc = "11.73 : Government deficit --> BLs[-1] missing in the book") |>
      add_equation("Bs = Bs[-1] + G - TX - (BLs - BLs[-1])*Pbl + Rb[-1]*(Bhs[-1] + Bbs[-1]) + BLs[-1]", desc = "11.74 : New issues of bills") |>
      add_equation("GD = Bbs + Bhs + BLs*Pbl + Hs", desc = "11.75 : Government debt") |>
      
      # The Central banks equations
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
      
      # Commercial Banks equations
      add_equation("Ms = Mh", desc = "11.87 : Bank deposits supplied on demand") |>
      add_equation("Lfs = Lfd", desc = "11.88 : Loans to firms supplied on demand") |>
      add_equation("Lhs = Lhd", desc = "11.89 : Personal loans supplied on demand") |>
      add_equation("Hbd = ro*Ms", desc = "11.90 Reserve requirements of banks") |>
      add_equation("Bbs = Bbs[-1] + (Bs - Bs[-1]) - (Bhs - Bhs[-1]) - (Bcbs - Bcbs[-1])", desc = "11.91 : Bills supplied to banks") |>
      add_equation("Bbd = Ms + OFb - Lfs - Lhs - Hbd", desc = "11.92 : Balance sheet constraint of banks") |>
      add_equation("BLR = Bbd/Ms", desc = "11.93 : Bank liquidity ratio") |>
      add_equation("Rm = Rm[-1] + z1a*xim1 + z1b*xim2 - z2a*xim1 - z2b*xim2", desc = "11.94 : Deposit interest rate") |>
      # 11.95-97 : Mechanism for determining changes to the interest rate on deposits,
      # i.e., logical functions dependent on whether the bank liquidity ratio is within its bot and top range.
      add_equation("z2a = (BLR[-1] >  (top + 0.05))", desc = "") |>
      add_equation("z2b = (BLR[-1] >  top)", desc = "") |>
      add_equation("z1a = 1 - as.numeric(BLR[-1] > bot)", desc = "") |>
      add_equation("z1b = 1 - as.numeric(BLR[-1] > (bot - 0.05))", desc = "") |>
      
      # Commercial banks equations
      add_equation("Rl = Rm + ADDl", desc = "11.98 : Loan interest rate") |>
      add_equation("OFbt = NCAR*(Lfs[-1] + Lhs[-1])", desc = "11.99 : Long-run own funds target") |>
      add_equation("OFbe = OFb[-1] + betab*(OFbt - OFb[-1])", desc = "11.100 : Short-run own funds target") |>
      add_equation("FUbt = OFbe - OFb[-1] + NPLke*Lfs[-1]", desc = "11.101 : Target retained earnings of banks") |>
      add_equation("NPLke = epsb*NPLke[-1] + (1 - epsb)*NPLk[-1]", desc = "11.102 : Expected proportion of non-performaing loans") |>
      add_equation("FDb = Fb - FUb", desc = "11.103 : Dividends of banks") |>
      add_equation("Fbt = lambdab*Y[-1] + (OFbe - OFb[-1] + NPLke*Lfs[-1])", desc = "11.104 : Target profits of banks") |>
      add_equation("Fb = Rl[-1]*(Lfs[-1] + Lhs[-1] - NPL) + Rb[-1]*Bbd[-1] - Rm[-1]*Ms[-1]", desc = "11.105 : Actual profits of banks") |>
      add_equation("ADDl = (Fbt - Rb[-1]*Bbd[-1] + Rm[-1]*(Ms[-1] - (1 - NPLke)*Lfs[-1] - Lhs[-1]))/((1 - NPLke)*Lfs[-1] + Lhs[-1])", desc = "11.106 : Lending mark-up over deposit rate --> we added the lag term to Rm") |>
      add_equation("FUb = Fb - lambdab*Y[-1]", desc = "11.107 : Actual retained earnings") |>
      add_equation("OFb = OFb[-1] + FUb - NPL", desc = "11.108 : Own funds of banks") |>
      add_equation("CAR = OFb/(Lfs + Lhs)", desc = "") |>
      add_equation("Vf = IN + K - Lfd - Ekd * Pe", desc = "Firm's wealth (memo for matrices)") |>
      add_equation("Ls = Lfs + Lhs", desc = "Loans supply (memo for matrices)") |>
      
      # Hidden equation
      add_equation("Bbs = Bbd", desc = "", hidden = TRUE)
  }
  else if (model_template_name == "OPENFIX") {
    model <- model |>
      # variables
      add_variable("alpha1_uk", init = 0.75) |>
      add_variable("alpha1_us", init = 0.75) |>
      add_variable("alpha2_uk", init = 0.13333) |>
      add_variable("alpha2_us", init = 0.13333) |>
      add_variable("eps0", init = -2.1) |>
      add_variable("eps1", init = 0.7) |>
      add_variable("eps2", init = 1) |>
      add_variable("lambda10", init = 0.7) |>
      add_variable("lambda11", init = 5) |>
      add_variable("lambda12", init = 5) |>
      add_variable("lambda20", init = 0.25) |>
      add_variable("lambda21", init = 5) |>
      add_variable("lambda22", init = 5) |>
      add_variable("lambda40", init = 0.7) |>
      add_variable("lambda41", init = 5) |>
      add_variable("lambda42", init = 5) |>
      add_variable("lambda50", init = 0.25) |>
      add_variable("lambda51", init = 5) |>
      add_variable("lambda52", init = 5) |>
      add_variable("mu0", init = -2.1) |>
      add_variable("mu1", init = 0.7) |>
      add_variable("mu2", init = 1) |>
      add_variable("nu0m", init = -0.00001) |>
      add_variable("nu0x", init = -0.00001) |>
      add_variable("nu1m", init = 0.7) |>
      add_variable("nu1x", init = 0.5) |>
      add_variable("phi_uk", init = 0.2381) |>
      add_variable("phi_us", init = 0.2381) |>
      add_variable("theta_uk", init = 0.2) |>
      add_variable("theta_us", init = 0.2) |>
      add_variable("b_cb_ukus_s", init = 0.02031) |>
      add_variable("dxre_us", init = 0) |>
      add_variable("g_k_uk", init = 16) |>
      add_variable("g_k_us", init = 16) |>
      add_variable("or_uk", init = 7) |>
      add_variable("pg_us", init = 1) |>
      add_variable("pr_uk", init = 1.3333) |>
      add_variable("pr_us", init = 1.3333) |>
      add_variable("r_uk", init = 0.03) |>
      add_variable("r_us", init = 0.03) |>
      add_variable("w_uk", init = 1) |>
      add_variable("w_us", init = 1) |>
      add_variable("b_cb_ukuk_d", init = 0.27984) |>
      add_variable("b_cb_ukuk_s", init = 0.27984) |>
      add_variable("b_cb_ukuk_sa", init = 0.27984) |>
      add_variable("b_cb_ukus_d", init = 0.0203) |>
      add_variable("b_cb_usus_d", init = 0.29843) |>
      add_variable("b_cb_usus_s", init = 0.29843) |>
      add_variable("b_uk_s", init = 138.94) |>
      add_variable("b_ukuk_d", init = 102.18) |>
      add_variable("b_ukuk_s", init = 102.18) |>
      add_variable("b_ukus_d", init = 36.493) |>
      add_variable("b_ukus_s", init = 36.504) |>
      add_variable("b_us_s", init = 139.02) |>
      add_variable("b_usuk_d", init = 36.497) |>
      add_variable("b_usuk_s", init = 36.487) |>
      add_variable("b_usus_d", init = 102.19) |>
      add_variable("b_usus_s", init = 102.19) |>
      add_variable("h_uk_d", init = 7.2987) |>
      add_variable("h_uk_s", init = 7.2987) |>
      add_variable("h_us_d", init = 7.2995) |>
      add_variable("h_us_s", init = 7.2995) |>
      add_variable("or_us", init = 7) |>
      add_variable("v_k_uk", init = 152.62) |>
      add_variable("v_k_us", init = 152.63) |>
      add_variable("v_uk", init = 145.97) |>
      add_variable("v_us", init = 145.99001) |>
      add_variable("c_k_uk", init = 81.393) |>
      add_variable("c_k_us", init = 81.401) |>
      add_variable("cab_uk", init = 0) |>
      add_variable("cab_us", init = 0) |>
      add_variable("cons_uk", init = 77.851) |>
      add_variable("cons_us", init = 77.86) |>
      add_variable("ds_k_uk", init = 97.393) |>
      add_variable("ds_k_us", init = 97.401) |>
      add_variable("ds_uk", init = 93.154) |>
      add_variable("ds_us", init = 93.164) |>
      add_variable("dxre_uk", init = 0) |>
      add_variable("f_cb_uk", init = 0.00869) |>
      add_variable("f_cb_us", init = 0.00895) |>
      add_variable("g_uk", init = 15.304) |>
      add_variable("g_us", init = 15.304) |>
      add_variable("im_k_uk", init = 11.928) |>
      add_variable("im_k_us", init = 11.926) |>
      add_variable("im_uk", init = 11.407) |>
      add_variable("im_us", init = 11.409) |>
      add_variable("kabp_uk", init = 0.00002) |>
      add_variable("kabp_us", init = -0.00002) |>
      add_variable("n_uk", init = 73.046) |>
      add_variable("n_us", init = 73.054) |>
      add_variable("pds_uk", init = 0.95648) |>
      add_variable("pds_us", init = 0.95649) |>
      add_variable("pg_uk", init = 0.99971) |>
      add_variable("pm_uk", init = 0.95628) |>
      add_variable("pm_us", init = 0.95661) |>
      add_variable("ps_uk", init = 0.95646) |>
      add_variable("ps_us", init = 0.9565) |>
      add_variable("px_uk", init = 0.95634) |>
      add_variable("px_us", init = 0.95656) |>
      add_variable("py_uk", init = 0.95648) |>
      add_variable("py_us", init = 0.95649) |>
      add_variable("s_k_uk", init = 109.32) |>
      add_variable("s_k_us", init = 109.33) |>
      add_variable("s_uk", init = 104.56) |>
      add_variable("s_us", init = 104.57) |>
      add_variable("t_uk", init = 19.463) |>
      add_variable("t_us", init = 19.465) |>
      add_variable("x_k_uk", init = 11.926) |>
      add_variable("x_k_us", init = 11.928) |>
      add_variable("x_uk", init = 11.406) |>
      add_variable("x_us", init = 11.41) |>
      add_variable("xr_uk", init = 1.0003) |>
      add_variable("xr_us", init = 0.99971) |>
      add_variable("xre_uk", init = 1.0003) |>
      add_variable("xre_us", init = 0.99971) |>
      add_variable("y_k_uk", init = 97.392) |>
      add_variable("y_k_us", init = 97.403) |>
      add_variable("y_uk", init = 93.154) |>
      add_variable("y_us", init = 93.164) |>
      add_variable("yd_uk", init = 77.851) |>
      add_variable("yd_us", init = 77.86) |>
      add_variable("ydhs_k_uk", init = 81.394) |>
      add_variable("ydhs_k_us", init = 81.402) |>
      add_variable("ydhse_k_uk", init = 81.394) |>
      add_variable("ydhse_k_us", init = 81.402) |>
      add_variable("yd_hs_uk") |>
      add_variable("yd_hs_us") |>
      add_variable("kab_uk") |>
      add_variable("kab_us") |>
      add_variable("psbr_uk") |>
      add_variable("psbr_us") |>
      add_variable("nafa_uk") |>
      add_variable("nafa_us") |>
      add_variable("nwcb_uk")
    
    # equations
    model <- model |>
      add_equation("yd_uk = (y_uk + r_uk[-1]*b_ukuk_d[-1] + xr_us*r_us[-1]*b_ukus_s[-1])*(1 - theta_uk) + (xr_us - xr_us[-1])*b_ukus_s[-1]", desc = "12.1 : Disposable income in UK") |>
      add_equation("yd_hs_uk = yd_uk + (xr_us - xr_us[-1])*b_ukus_s[-1]", desc = "12.2 : Haig-Simons disposable income in UK") |>
      add_equation("v_uk = v_uk[-1] + yd_uk - cons_uk", desc = "12.3 : Wealth accumulation in UK") |>
      add_equation("yd_us = (y_us + r_us[-1]*b_usus_d[-1] + xr_uk*r_uk[-1]*b_usuk_s[-1])*(1 - theta_us) + (xr_uk - xr_uk[-1])*b_usuk_s[-1]", desc = "12.4 : Disposable income in US") |>
      add_equation("yd_hs_us = yd_us + d(xr_uk)*b_usuk_s[-1]", desc = "12.5 : Haig-Simons disposable income in US") |>
      add_equation("v_us = v_us[-1] + yd_us - cons_us", desc = "12.6 : Wealth accumulation in US") |>
      add_equation("t_uk = theta_uk*(y_uk + r_uk[-1]*b_ukuk_d[-1] + xr_us*r_us[-1]*b_ukus_s[-1])", desc = "12.7 : Taxes in UK") |>
      add_equation("t_us = theta_us*(y_us + r_us[-1]*b_usus_d[-1] + xr_uk*r_uk[-1]*b_usuk_s[-1])", desc = "12.8 : Taxes in US") |>
      add_equation("f_cb_uk = r_uk[-1]*b_cb_ukuk_d[-1] + r_us[-1]*b_cb_ukus_s[-1]*xr_us", desc = "12.11 : Profits of Central Bank in UK") |>
      add_equation("f_cb_us = r_us[-1]*b_cb_usus_d[-1]", desc = "12.12 : Profits of Central Bank in US") |>
      add_equation("b_uk_s = b_uk_s[-1] + g_uk + r_uk[-1]*b_uk_s[-1] - t_uk - f_cb_uk", desc = "12.13 : Government budget constraint - UK") |>
      add_equation("b_us_s = b_us_s[-1] + g_us + r_us[-1]*b_us_s[-1] - t_us - f_cb_us", desc = "12.14 : Government budget constraint - US") |>
      add_equation("cab_uk = x_uk - im_uk + xr_us*r_us[-1]*b_ukus_s[-1] - r_uk[-1]*b_usuk_s[-1] + r_us[-1]*b_cb_ukus_s[-1]*xr_us", desc = "12.15 : Current account balance - UK") |>
      add_equation("kab_uk = kabp_uk - (xr_us*(b_cb_ukus_s - b_cb_ukus_s[-1]) + pg_uk*(or_uk - or_uk[-1]))", desc = "12.16 : Capital account balance in UK") |>
      add_equation("cab_us = x_us - im_us + xr_uk*r_uk[-1]*b_usuk_s[-1] - r_us[-1]*b_ukus_s[-1] - r_us[-1]*b_cb_ukus_s[-1]", desc = "12.17 : Current account balance in US") |>
      add_equation("kab_us = kabp_us + (b_cb_ukus_s - b_cb_ukus_s[-1]) - pg_us*(or_us - or_us[-1])", desc = "12.18 : Capital account balance in US") |>
      add_equation("kabp_uk = -(b_ukus_s - b_ukus_s[-1])*xr_us + (b_usuk_s - b_usuk_s[-1])", desc = "12.19 : Capital account balance in UK, net of official transactions") |>
      add_equation("kabp_us = -(b_usuk_s - b_usuk_s[-1])*xr_uk + (b_ukus_s - b_ukus_s[-1])", desc = "12.20 : Capital account balance in US, net of official transactions") |>
      
      # Trade
      add_equation("pm_uk = exp(nu0m + nu1m*log(py_us) + (1 - nu1m)*log(py_uk) - nu1m*log(xr_uk))", desc = "12.21 : Import prices in UK") |>
      add_equation("px_uk = exp(nu0x + nu1x*log(py_us) + (1 - nu1x)*log(py_uk) - nu1x*log(xr_uk))", desc = "12.22 : Export prices in UK") |>
      add_equation("px_us = pm_uk*xr_uk", desc = "12.23 : Export prices in US") |>
      add_equation("pm_us = px_uk*xr_uk", desc = "12.24 : Import prices in US") |>
      add_equation("x_k_uk = exp(eps0 - eps1*log(pm_us/py_us) + eps2*log(y_k_us))", desc = "12.25 : Real exports from UK - depends on current relative price") |>
      add_equation("im_k_uk = exp(mu0 - mu1*log(pm_uk[-1]/py_uk[-1]) + mu2*log(y_k_uk))", desc = "12.26 : Real imports of UK") |>
      add_equation("x_k_us = im_k_uk", desc = "12.27 : Real exports from US") |>
      add_equation("im_k_us = x_k_uk", desc = "12.28 : Real imports of US") |>
      add_equation("x_uk = x_k_uk*px_uk", desc = "12.29 : Exports of UK") |>
      add_equation("x_us = x_k_us*px_us", desc = "12.30 : Exports of US") |>
      add_equation("im_uk = im_k_uk*pm_uk", desc = "12.31 : Imports of UK") |>
      add_equation("im_us = im_k_us*pm_us", desc = "12.32 : Imports of US") |>
      
      # Income and expenditure
      add_equation("v_k_uk = v_uk/pds_uk", desc = "12.33 : Real wealth in UK") |>
      add_equation("v_k_us = v_us/pds_us", desc = "12.34 : Real wealth in US") |>
      add_equation("ydhs_k_uk = yd_uk/pds_uk - v_k_uk[-1]*(pds_uk - pds_uk[-1])/pds_uk", desc = "12.35 : Real Haig-Simons disposable income in UK") |>
      add_equation("ydhs_k_us = yd_us/pds_us - v_k_us[-1]*(pds_us - pds_us[-1])/pds_us", desc = "12.36 : Real Haig-Simons disposable income in US") |>
      add_equation("c_k_uk = alpha1_uk*ydhse_k_uk + alpha2_uk*v_k_uk[-1]", desc = "12.37 : Real consumption in UK") |>
      add_equation("c_k_us = alpha1_us*ydhse_k_us + alpha2_us*v_k_us[-1]", desc = "12.38 : Real consumption in US") |>
      add_equation("ydhse_k_uk = (ydhs_k_uk + ydhs_k_uk[-1])/2", desc = "12.39 : Expected real Haig-Simons disposable income in UK") |>
      add_equation("ydhse_k_us = (ydhs_k_us + ydhs_k_us[-1])/2", desc = "12.40 : Expected real Haig-Simons disposable income in US") |>
      add_equation("s_k_uk = c_k_uk + g_k_uk + x_k_uk", desc = "12.41 : Real sales in UK") |>
      add_equation("s_k_us = c_k_us + g_k_us + x_k_us", desc = "12.42 : Real sales in US") |>
      add_equation("s_uk = s_k_uk*ps_uk", desc = "12.43 : Value of sales in UK") |>
      add_equation("s_us = s_k_us*ps_us", desc = "12.44 : Value of sales in US") |>
      add_equation("ps_uk = (1 + phi_uk)*(w_uk*n_uk + im_uk)/s_k_uk", desc = "12.45 : Price of sales in UK") |>
      add_equation("ps_us = (1 + phi_us)*(w_us*n_us + im_us)/s_k_us", desc = "12.46 : Price of sales in US") |>
      add_equation("pds_uk = (s_uk - x_uk)/(s_k_uk - x_k_uk)", desc = "12.47 : Price of domestic sales in UK") |>
      add_equation("pds_us = (s_us - x_us)/(s_k_us - x_k_us)", desc = "12.48 : Price of domestic sales in US") |>
      add_equation("ds_uk = s_uk - x_uk", desc = "12.49 : Domestic sales in UK") |>
      add_equation("ds_us = s_us - x_us", desc = "12.50 : Domestic sales in US") |>
      add_equation("ds_k_uk = c_k_uk + g_k_uk", desc = "12.51 : Real domestic sales in UK") |>
      add_equation("ds_k_us = c_k_us + g_k_us", desc = "12.52 : Real domestic sales in US") |>
      add_equation("y_uk = s_uk - im_uk", desc = "12.53 : Value of output in UK") |>
      add_equation("y_us = s_us - im_us", desc = "12.54 : Value of output in US") |>
      
      add_equation("y_k_uk = s_k_uk - im_k_uk", desc = "12.55 : Value of real output in UK") |>
      add_equation("y_k_us = s_k_us - im_k_us", desc = "12.56 : Value of real output in US") |>
      add_equation("py_uk = y_uk/y_k_uk", desc = "12.57 : Price of output in UK") |>
      add_equation("py_us = y_us/y_k_us", desc = "12.58 : Price of output in US") |>
      add_equation("cons_uk = c_k_uk*pds_uk", desc = "12.59 : Consumption in UK") |>
      add_equation("cons_us = c_k_us*pds_us", desc = "12.60 : Consumption in US") |>
      add_equation("g_uk = g_k_uk*pds_uk", desc = "12.61 : Government expenditure in UK") |>
      add_equation("g_us = g_k_us*pds_us", desc = "12.62 : Government expenditure in US") |>
      add_equation("n_uk = y_k_uk/pr_uk", desc = "12.65 : Employment in UK") |>
      add_equation("n_us = y_k_us/pr_us", desc = "12.66 : Employment in US") |>
      
      # Asset demands
      add_equation("b_ukuk_d = v_uk*(lambda10 + lambda11*r_uk - lambda12*(r_us + dxre_us))", desc = "12.67 : Demand for UK bills in UK") |>
      add_equation("h_uk_d = v_uk - b_ukuk_d - b_ukus_d", desc = "12.69 : Demand for money in UK") |>
      add_equation("b_usus_d = v_us*(lambda40 + lambda41*r_us - lambda42*(r_uk + dxre_uk))", desc = "12.70 : Demand for US	bills in US") |>
      add_equation("b_usuk_d = v_us*(lambda50 - lambda51*r_us + lambda52*(r_uk + dxre_uk))", desc = "12.71 : Demand for UK bills in US") |>
      add_equation("h_us_d = v_us - b_usus_d - b_usuk_d", desc = "12.72 : Demand for money in US") |>
      
      # Asset supplies
      add_equation("h_us_s = h_us_d", desc = "12.77 : Suply of cash in US") |>
      add_equation("b_usus_s = b_usus_d", desc = "12.78 : Supply of US bills to Country") |>
      add_equation("b_cb_usus_s = b_cb_usus_d", desc = "12.79 : Supply of US bills to US Central bank") |>
      add_equation("h_uk_s = h_uk_d", desc = "12.80 : Suply of cash in UK") |>
      add_equation("b_ukuk_s = b_ukuk_d", desc = "12.81 : Bills issued by US acquired by US") |>
      add_equation("b_cb_ukuk_s = b_cb_ukuk_d", desc = "12.82 : Supply of UK bills to UK Central bank") |>
      add_equation("b_cb_usus_d = b_cb_usus_d[-1] + (h_us_s - h_us_s[-1]) - (or_us - or_us[-1])*pg_us", desc = "12.83 : Balance sheet of US Central bank - expressed as changes") |>
      add_equation("b_cb_ukuk_d = b_cb_ukuk_d[-1] + (h_uk_s - h_uk_s[-1]) - (b_cb_ukus_s - b_cb_ukus_s[-1])*xr_us - (or_uk - or_uk[-1])*pg_uk", desc = "12.84 : Balance sheet of UK Central bank") |>
      add_equation("pg_uk = pg_us/xr_uk", desc = "12.85 : Price of gold is equal in the two countries") |>
      add_equation("xr_us = 1/xr_uk", desc = "12.86 : US exchange rate") |>
      add_equation("b_usuk_s = b_usuk_d*xr_us", desc = "12.87 : Equilibrium condition for bills issued by UK acquired by US") |>
      add_equation("b_cb_ukus_d = b_cb_ukus_s*xr_us", desc = "12.88 : Equilibrium condition for bills issued by US acquired by UK Central bank") |>
      add_equation("psbr_uk = g_uk + r_uk[-1]*b_uk_s[-1] - t_uk - f_cb_uk", desc = "Government deficit in the UK") |>
      add_equation("psbr_us = g_us + r_us[-1]*b_us_s[-1] - t_us - f_cb_us", desc = "Government deficit in the US") |>
      add_equation("nafa_uk = psbr_uk + cab_uk", desc = "Net accumulation of financial assets in the UK") |>
      add_equation("nafa_us = psbr_us + cab_us", desc = "Net accumulation of financial assets in the US") |>
      add_equation("b_cb_ukuk_sa = b_uk_s - b_ukuk_s - b_usuk_s", desc = "") |>
      add_equation("nwcb_uk = -h_uk_d + b_cb_ukuk_d + b_cb_ukus_d * xr_us + or_uk * pg_uk", desc = "Net wealth of CBUK") |>
      
      # Model OPEN FIX R closure
      add_equation("b_ukus_d = b_ukus_s*xr_us", desc = "12.89R : Demand of UK Bills in US") |>
      add_equation("b_ukus_s =  b_us_s - b_usus_s - b_cb_usus_d - b_cb_ukus_s", desc = "12.90R : Supply of UK bills to US") |>
      add_equation("r_uk = (lambda20 + lambda22*(r_us + dxre_us) - b_ukus_d/v_uk)/lambda21", desc = "12.68R: Endogenous interest rate")
  }
  
  return(model)
}
