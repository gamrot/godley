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

    ## equations
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

    ## equations
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

    ## equations
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

    # Add equations
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

    # Add equations
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

    # Add equations
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

    # Add equations
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


    # Add equations
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

  return(model)
}
