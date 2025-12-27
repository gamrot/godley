# model GROWTH

# Configuration
if (!require(here)) install.packages("here")
here::i_am("godley/vignettes/examples/14_GROWTH.R")
source(here::here("godley/vignettes/examples/dependencies/00_growth.R"))

# Create empty model
model_growth <- create_model(name = "SFC GROWTH")

# Add variables
model_growth <- model_growth |>
  # growth_parameters
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
  
  # Exogenous
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
  
  # growth_initial
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
  add_variable("Vf", init = 31361792)

# Add equations
model_growth <- model_growth |>
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
  # add_equation("YP = WB + FDf + FDb + Rm[-1]*Md[-1] + Rb[-1]*Bhd[-1] + BLs[-1]", desc = "11.45 : Personal income") |>
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
  add_equation("Bbs = Bbd", desc = "", hidden = TRUE)

# Add variables and equations used in the sfcr model but not explicitly declared
model_growth <- model_growth |>
  # Variables without initial values.
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

# Simulate model
model_growth <- simulate_scenario(model_growth, scenario = "baseline", 
                                  max_iter = 350, periods = 500, tol = 1e-15,
                                  hidden_tol = 1e-6, rhtol = TRUE, method = "Broyden")

# Steady state
exprs <- c("Bsk = Bs / K", "VK = V / K", "GRk", "PI")
plots <- map(exprs,
             ~ plot_simulation(model = model_growth, scenario = "baseline",
                               from = 1, to = 350, expressions = .x
             )
)
subplot(plots, nrows = 2, shareX = TRUE, titleX = TRUE)

# A steady state from about t = 300 onward.
t0 <- 300

# Structure of the model (DAG)
plot_cycles(model_growth)

# Scenario 1: Autonomous increase in the target real wage
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "omega0", value = -0.1, start = 5, end = 150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "omega0_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "omega0_shock", periods = 150, 
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.2A
plot_simulation(model = model_growth, scenario = c("baseline", "omega0_shock"),
                from = 1, to = 150, expressions = c("PI", "wi = (W - dplyr::lag(W)) / dplyr::lag(W)"))

# Figure 11.2B
do_cplot(m = model_growth, scenario = "omega0_shock",
         from = 1, to = 150, variables = c("Ik", "Ck", "Yk"))


# Scenario 1., second experiment
# Add shock equation
shock_growth <- shock_growth |>
  add_shock(variable = "Rbbar", value = 0.055, start = 5, end = 150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "Rbbar_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "Rbbar_shock", periods = 500,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.2C
plot_simulation(model = model_growth, scenario = c("Rbbar_shock"),
                from = 1, to = 150, expressions = c("Rrb = Rb - PI"))

# Figure 11.2D
do_cplot(m = model_growth, scenario = "Rbbar_shock",
         from = 1, to = 150, variables = c("Yk"))

# Scenario 2: One-period only increase in the growth rate of pure government expenditures
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "GRg", value = 0.035, start = t0+10, end = t0+11, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "GRg_onetime_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "GRg_onetime_shock", periods = t0+150,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.3A
do_cplot(m = model_growth, scenario = "GRg_onetime_shock",
         variables = c("Yk", "Gk"), plot = c("Yk", "Gk"),
         from = t0, to = t0+150)

# Figure 11.3B
do_cplot(m = model_growth, scenario = "GRg_onetime_shock",
         variables = c("ER"), plot = NULL,
         from = t0, to = t0+150) +
  map(c(1.02, 0.98), ~geom_hline(yintercept = .x))

# Figure 11.3C
do_cplot(m = model_growth, scenario = "GRg_onetime_shock",
         variables = c("PSBR", "Y", "GD"), plot = c("DefY", "GDY"),
         from = t0, to = t0+150)

# Figure 11.3D
do_cplot(m = model_growth, scenario = "GRg_onetime_shock",
         variables = c("Lfd", "IN", "BLR"), plot = c("LIN", "BLR"),
         from = t0, to = t0+150)

# Scenario 2.B: One-shot decrease in the income tax
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "theta", value = 0.22, start = t0+10, end = t0+150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "theta_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "theta_shock", periods = t0+150,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.3E
do_cplot(m = model_growth, scenario = "theta_shock",
         variables = c("Ck", "Yk"), plot = c("Ck", "Yk"),
         from = t0, to = t0+150)

# Scenario 3: A permanent increase in the growth rate of pure government expenditures
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "GRg", value = 0.035, start = t0+10, end = t0+150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "GRg_permanent_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "GRg_permanent_shock", periods = t0+150,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.4A
exprs <- c("ER", "PI")
plots <- map(exprs,
             ~ plot_simulation(model = model_growth, scenario = "GRg_permanent_shock",
                               from = t0, to = t0+150, expressions = .x))
subplot(plots, nrows = 1, shareX = TRUE, titleX = TRUE)

# Figure 11.4B
plot_simulation(model = model_growth, scenario = "GRg_permanent_shock",
                from = t0, to = t0+150, expressions = c("GRg", "GRk", "GRy = (Yk / dplyr::lag(Yk)) - 1")
                )

# Figure 11.4C
exprs <- c("DefY = PSBR/Y", "GDY = GD/Y")
plots <- map(exprs,
             ~ plot_simulation(model = model_growth, scenario = "GRg_permanent_shock",
                               from = t0, to = t0+150, expressions = .x))
subplot(plots, nrows = 1, shareX = TRUE, titleX = TRUE)

# # Scenario 4: A permanent increase in the bill rate of interest
# # Create empty shock and add shock equation
# shock_growth <- create_shock() |>
#   add_shock(variable = "Rbbar", value = 0.038, start = t0+10, end = t0+11, desc = "") |>
#   add_shock(variable = "Rbbar", value = 0.041, start = t0+11, end = t0+12, desc = "") |>
#   add_shock(variable = "Rbbar", value = 0.044, start = t0+13, end = t0+14, desc = "") |>
#   add_shock(variable = "Rbbar", value = 0.047, start = t0+15, end = t0+150, desc = "")
# 
# model_growth <- model_growth |>
#   add_scenario(name = "Rbbar_permanent_shock", origin = "baseline", shock = shock_growth)
# 
# # Simulate shock
# model_growth <- simulate_scenario(model_growth, scenario = "Rbbar_permanent_shock", periods = t0+150,
#                                   max_iter = 350, tol = 1e-10, method = "Broyden",
#                                   hidden_tol = 1e-6, rhtol = TRUE)
# # Plot results
# # Figure 11.5A
# plot_simulation(model = model_growth, scenario = "Rbbar_permanent_shock",
#                 from = t0, to = t0+80, expressions = c("Rl", "Rbl", "Rb", "Rm")
# )
# 
# # Figure 11.5B
# do_cplot(m = model_growth, scenario = "Rbbar_permanent_shock",
#          variables = c("Ck", "Yk"), plot = c("Ck", "Yk"),
#          from = t0, to = t0+150)
# 
# # Figure 11.5C
# do_cplot(m = model_growth, scenario = "Rbbar_permanent_shock",
#          variables = c("GD", "Y"), plot = c("GDY"),
#          from = t0, to = t0+150)
# 
# # Figure 11.5D
# plot_simulation(model = model_growth, scenario = "Rbbar_permanent_shock",
#                 from = t0, to = t0+150, expressions = c("LhYDr = Lhd / YDr")
# )
# 
# # Figure 11.5E
# plot_simulation(model = model_growth, scenario = "Rbbar_permanent_shock",
#                 from = t0, to = t0+150, expressions = c("BUR")
# )

# Scenario 5: Increase in the propensity to consume out of regular income
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "alpha1", value = 0.80, start = t0+10, end = t0+150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "alpha1_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "alpha1_shock", periods = t0+150,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.7A
do_cplot(m = model_growth, scenario = "alpha1_shock",
         variables = c("Ck", "Yk"), plot = c("Ck", "Yk"),
         from = t0, to = t0+150)

# Figure 11.7B
do_cplot(m = model_growth, scenario = "alpha1_shock",
         variables = c("Vk"), plot = c("Vk"),
         from = t0, to = t0+150)

# Figure 11.7C
do_cplot(m = model_growth, scenario = "alpha1_shock",
         variables = c("PI"), plot = c("PI"),
         from = t0, to = t0+150)

# Figure 11.7D
do_cplot(m = model_growth, scenario = "alpha1_shock",
         variables = c("NHUC", "phi"), plot = c("NHUC", "phi"),
         from = t0, to = t0+150)

# Figure 11.7E
do_cplot(m = model_growth, scenario = "alpha1_shock",
         variables = c("FUf", "INV", "INk"), plot = c("FUfInv", "INk"),
         from = t0, to = t0+150)

# Figure 11.7F
do_cplot(m = model_growth, scenario = "alpha1_shock",
         variables = c("Y", "GD", "PSBR"), plot = c("DefY", "GDY"),
         from = t0, to = t0+150)

# Figure 11.7G
do_cplot(m = model_growth, scenario = "alpha1_shock",
         variables = c("Q", "PE"), plot = c("Q", "PE"),
         from = t0, to = t0+150)

# Scenario 7: An increase in the gross new loans to personal income ratio
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "eta0", value = 0.08416, start = t0+10, end = t0+150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "eta0_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "eta0_shock", periods = t0+150,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.8A
exprs <- c("LhYDr = Lhd / YDr", "BUR")
plots <- map(exprs,
             ~ plot_simulation(model = model_growth, scenario = "eta0_shock",
                               from = t0, to = t0+150, expressions = .x))
subplot(plots, nrows = 1, shareX = TRUE, titleX = TRUE)

# Figure 11.8B
do_cplot(m = model_growth, scenario = "eta0_shock",
         variables = c("Yk", "Ck"), plot = c("Yk", "Ck"),
         from = t0, to = t0+150)

# Figure 11.8C
do_cplot(m = model_growth, scenario = "eta0_shock",
         variables = c("BLR", "CAR"), plot = c("BLR", "CAR"),
         from = t0, to = t0+150)

# Figure 11.8D
plot_simulation(model = model_growth, scenario = "eta0_shock",
                from = t0, to = t0+150, expressions = c("Rl"))

# Figure 11.8E
do_cplot(m = model_growth, scenario = "eta0_shock",
         variables = c("Y", "PSBR", "GD"), plot = c("DefY", "GDY"),
         from = t0, to = t0+150)

# Scenario 8: An increase in the desire to hold equities
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "lambda40", value = 0.77132, start = t0+10, end = t0+150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "lambda40_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "lambda40_shock", periods = t0+150,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.9A
do_cplot(m = model_growth, scenario = "lambda40_shock",
         variables = c("Q", "PE", "Ekd", "Pe", "Vfma"), plot = c("Q", "PE", "EqFMA"),
         from = t0, to = t0+150)

# Figure 11.9B
do_cplot(m = model_growth, scenario = "lambda40_shock",
         variables = c("Ck", "Vk", "Yk", "Ik"), plot = c("Ck", "Vk", "Yk", "Ik"),
         from = t0, to = t0+150)

# Figure 11.9C
plot_simulation(model = model_growth, scenario = "lambda40_shock",
                from = t0, to = t0+150, expressions = c("Rl", "Rm"))

# # Scenario 8b: Increase in the desire to hold equities that is offset by a decline in the desire to hold bills and bonds
# # Create empty shock and add shock equation
# shock_growth <- create_shock() |>
#   add_shock(variable = "lambda20", value = 0.20, start = t0+10, end = t0+150, desc = "") |>
#   add_shock(variable = "lambda30", value = -0.09341, start = t0+10, end = t0+150, desc = "")
# 
# model_growth <- model_growth |>
#   add_scenario(name = "lambda20-30_shock", origin = "baseline", shock = shock_growth)
# 
# # Simulate shock
# model_growth <- simulate_scenario(model_growth, scenario = "lambda20-30_shock", periods = t0+150,
#                                   max_iter = 350, tol = 1e-10, method = "Broyden",
#                                   hidden_tol = 1e-6, rhtol = TRUE)
# # Plot results
# # Figure 11.9D
# plot_simulation(model = model_growth, scenario = "lambda20-30_shock",
#                 from = t0, to = t0+150, expressions = c("Rl", "Rb", "Rm"))
# 

# Scenario 9: An increase in the target proportion of gross investment financed by retained earnings
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "psiu", value = 1, start = t0+10, end = t0+150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "psiu_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "psiu_shock", periods = t0+150,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.10A
plot_simulation(model = model_growth, scenario = "psiu_shock",
                from = t0, to = t0+150, expressions = c("phi"))

# Figure 11.10B
plot_simulation(model = model_growth, scenario = "psiu_shock",
                from = t0, to = t0+150, expressions = c("wi = (W - dplyr::lag(W)) / dplyr::lag(W)"))

# Figure 11.10C
do_cplot(m = model_growth, scenario = "psiu_shock",
         variables = c("ER", "Ck"), plot = c("ER", "Ck"),
         from = t0, to = t0+150)

# Figure 11.10D
do_cplot(m = model_growth, scenario = "psiu_shock",
         variables = c("Q", "PE"), plot = c("Q", "PE"),
         from = t0, to = t0+150)

# Figure 11.10E
plot_simulation(m = model_growth, scenario = "psiu_shock",
                from = t0, to = t0+150,
                expressions = c(
                  "GRFfk = -1 + (Ff / P)/(dplyr::lag(Ff)/dplyr::lag(P))",
                  "GRPek = -1 + (Pe / P) / (dplyr::lag(Pe)/dplyr::lag(P))"
                  ))

# Scenario 10: An increase in non-performing loans
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "NPLk", value = 0.05, start = t0+10, end = t0+150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "NPLk_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "NPLk_shock", periods = t0+150,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.11A
plot_simulation(model = model_growth, scenario = "NPLk_shock",
                from = t0, to = t0+150, expressions = c("CAR"))

# Figure 11.11B
do_cplot(m = model_growth, scenario = "NPLk_shock",
         variables = c("Rm", "Rl"), plot = c("Rm", "Rl"),
         from = t0, to = t0+150)

# Scenario 10B: An increase in the normal adequacy ratio
# Create empty shock and add shock equation
shock_growth <- create_shock() |>
  add_shock(variable = "NCAR", value = 0.11, start = t0+10, end = t0+150, desc = "")

model_growth <- model_growth |>
  add_scenario(name = "NCAR_shock", origin = "baseline", shock = shock_growth)

# Simulate shock
model_growth <- simulate_scenario(model_growth, scenario = "NCAR_shock", periods = t0+150,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 1e-6, rhtol = TRUE)
# Plot results
# Figure 11.11C
plot_simulation(model = model_growth, scenario = "NCAR_shock",
                from = t0, to = t0+150, expressions = c("NCAR", "CAR"))

# Figure 11.11D
plot_simulation(model = model_growth, scenario = "NCAR_shock",
                from = t0, to = t0+150, expressions = c("Rl"))

