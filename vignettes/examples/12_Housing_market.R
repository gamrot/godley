model_lp1 <- create_model(name = "Housing sfc model")


# Add variables
model_lp1 <- model_lp1 %>%
  add_variable("Y", desc = "National income") %>%
  add_variable("YDcr", desc = "Regular Disposable Income - capitalist") %>%
  add_variable("YDwr", desc = "Regular Disposable Income - workers") %>%
  add_variable("T", desc = "Taxes") %>%
  add_variable("V", desc = "Wealth") %>%
  add_variable("Vc", init = 40, desc = "Wealth - capitalists") %>%
  add_variable("Vw", init = 40, desc = "Wealth - workers") %>%
  add_variable("CGc", desc = "Capital Gains - capitalists") %>%
  add_variable("C", desc = "Consumption") %>%
  add_variable("Cc", desc = "Consumption - capitalists") %>%
  add_variable("Cw", desc = "Consumption - workers") %>%
  add_variable("Vce", desc = "Expected wealth - capitalists") %>%
  add_variable("Dch", desc = "Deposits held by capitalsts") %>%
  add_variable("Dwh", desc = "Deposits held by workers") %>%
  add_variable("BLcd", desc = "Demand for gov bonds") %>%
  add_variable("Hcd", init = 0.1, desc = "Total capitalist demand for houses") %>%
  add_variable("Mh", desc = "Mortgages held by banks") %>%
  add_variable("PB", desc = "Profits of banks") %>%
  add_variable("D", desc = "Deposits") %>%
  add_variable("BLch", desc = "Bonds held by capitalists") %>%
  add_variable("BLbh", desc = "Bonds held by banks") %>%
  add_variable("Ms", desc = "Supply of mortgages") %>%
  add_variable("Md", desc = "Demand for mortgages") %>%
  add_variable("Ds", desc = "Deposits supplied by commercial banks") %>%
  add_variable("BLs", desc = "Supply of gov bonds") %>%
  add_variable("ErBL", desc = "Expected rate of return on bonds") %>%
  add_variable("rBL", desc = "Interest rate on bonds") %>%
  add_variable("YDer", desc = "Expected regular disposable income") %>%
  add_variable("YDcer", desc = "Expected regular disposable income - capitalists") %>%
  add_variable("YDwer", desc = "Expected regular disposable income - workers") %>%
  add_variable("NHd", desc = "Demand for new houses") %>%
  add_variable("NHcd", desc = "Demand for new houses - capitalists") %>%
  add_variable("NHwd", desc = "Demand for new houses - workers") %>%
  add_variable("Hs", init = 6, desc = "Supply of houses") %>%
  add_variable("NHch", desc = "New houses held by capitalists") %>%
  add_variable("NHwh", desc = "New houses held by workers") %>%
  add_variable("NHgh", desc = "New houses held by gov") %>%
  add_variable("ph", init = 1, desc = "Price of house") %>%
  add_variable("phg", init = 0.6, desc = "Price of house paid by government") %>%
  add_variable("phc", init = 1, desc = "Price of house paid by non-government") %>%
  add_variable("CGd", desc = "Capital gains on houses") %>%
  add_variable("Hc", init = 0.5, desc = "Total number of houses held by capitalists") %>%
  add_variable("Hw", desc = "Total number of houses held by workers") %>%
  add_variable("Hg", desc = "Total number of houses held by gov") %>%
  add_variable("Hh", desc = "New houses held") %>%
  add_variable("Hu", desc = "Unsold houses") %>%
  add_variable("ERrd", desc = "Expected rate of return on houses") %>%
  add_variable("rh", desc = "Interest rate on houses") %>%
  add_variable("rentc", desc = "Rents paid by workers to capitalists") %>%
  add_variable("rentg", desc = "Rents paid by workers to government") %>%
  add_variable("W", desc = "Wages in housing sector") %>%
  add_variable("P", desc = "Capitalists profits from production of houses") %>%
  add_variable("G", init = 60, desc = "Public Expenditures") %>%
  add_variable("Trate", init = 0.05, desc = "Tax rate") %>%
  add_variable("alpha1", init = 0.8, desc = "Marginal propensity to consume (income)") %>%
  add_variable("alpha2", init = 0.2, desc = "Marginal propensity to consume (wealth)") %>%
  add_variable("lam20", init = 0.44196, desc = "Portfolio parameter") %>%
  add_variable("lam22", init = 1.1, desc = "Portfolio parameter") %>%
  add_variable("lam23", init = 1, desc = "Portfolio parameter") %>%
  add_variable("lam24", init = 0.03, desc = "Portfolio parameter") %>%
  add_variable("lam30", init = 0.3997, desc = "Portfolio parameter") %>%
  add_variable("lam32", init = 1, desc = "Portfolio parameter") %>%
  add_variable("lam33", init = 1.1, desc = "Portfolio parameter") %>%
  add_variable("lam34", init = 0.03, desc = "Portfolio parameter") %>%
  add_variable("chi", init = 0.1, desc = "Expectation about bonds price") %>%
  add_variable("rd", init = 0, desc = "Rate of interest of deposits") %>%
  add_variable("rb", init = 0.03, desc = "Rate of interest of mortgages") %>%
  add_variable("pBL", init = 50, desc = "Price of bonds") %>%
  add_variable("Z", init = 0.1, desc = "Expectation about house prices") %>%
  add_variable("NHgd", init = 1, desc = "Gov demand for new houses") %>%
  add_variable("rg", init = 0.02, desc = "Rents of public houses") %>%
  add_variable("phparam", init = 0.0005, desc = "House price elasticity to unsold houses") %>%
  add_variable("dsparam", init = 0.02, desc = "House supply elasticity to price") %>%
  add_variable("phgparam", init = 1.01, desc = "Price of houses paid by government relative to commercial price") %>%
  add_variable("Wparam", init = 0.9, desc = "Share of costs of houses production paid by government") %>%
  add_variable("mparam", init = 0.1, desc = "Share of workers disposable income which could be spent on mortgages") %>%
  add_variable("hsparam", init = 1.5, desc = "Price elasticity of supply of houses")



model_lp1 <- model_lp1 %>%
  add_equation("Y=C+G") %>%
  add_equation("YDcr=0.5*(Y-T)+PB+rBL*BLch[-1]+rentc+P+rd*Dch") %>% #
  add_equation("rentc = Hc[-1]*phc[-1]*rh[-1]") %>%
  add_equation("P=Hh*ph-W") %>%
  add_equation("W=Wparam*phg*Hs") %>%
  add_equation("rentg = Hg[-1]*rg[-1]*phg[-1]") %>%
  add_equation("YDwr=0.5*(Y-T) - rb*Mh[-1] - (rh*Mh[-1] + sqrt(Mh[-1])) - rentg - rentc + W + rd*Dwh") %>%
  add_equation("T=Trate*Y") %>%
  add_equation("V=Vc+Vw") %>%
  add_equation("Vc=Vc[-1]+(YDcr-Cc) + CGc") %>%
  add_equation("Vw=Vw[-1]+(YDwr-Cw) + Hw[-1]*(ph-ph[-1])-Mh") %>%
  add_equation("CGc=(pBL-pBL[-1])*BLch[-1]+(ph-ph[-1])*Hc[-1]") %>%
  add_equation("C=Cc+Cw") %>%
  add_equation("Cc=(alpha1*YDcer)+(alpha2*Vc[-1])") %>% #
  add_equation("Cw=(alpha1*YDwer)+(alpha2*Vw[-1])") %>%
  add_equation("Vce=Vc[-1]+(YDcer-Cc)") %>%
  add_equation("Dch=Vc-BLcd*pBL-Hc*ph") %>%
  add_equation("Dwh=Vw-Hw*ph") %>%
  add_equation("D=Dwh+Dch") %>%
  add_equation("BLcd=(Vce*lam20+Vce*ErBL*lam22-Vce*ERrd*lam23-lam24*YDcer)/pBL") %>%
  add_equation("Hcd= (Vce*lam30-Vce*ErBL*lam32+Vce*ERrd*lam33-lam34*YDcer)/phc") %>%
  add_equation("Mh=max(Mh[-1]-(rh*Mh[-1]+sqrt(Mh[-1]))+NHwh*ph,0)") %>%
  add_equation("BLch=max(BLcd,0)") %>%
  add_equation("BLbh=BLs-BLch") %>%
  add_equation("PB=rb[-1]*Mh[-1]+rBL*BLbh[-1]-rd[-1]*D[-1]") %>%
  add_equation("Ms=mparam*YDwr[-1]") %>%
  add_equation("Ds=Dch+Dwh") %>%
  add_equation("BLs=G-rentg-T+rBL*BLch[-1]") %>%
  add_equation("ErBL=rBL[-1]") %>%
  add_equation("rBL=1/pBL") %>%
  add_equation("YDer=YDcr[-1]+YDwr[-1]") %>%
  add_equation("YDcer=YDcr[-1]") %>%
  add_equation("YDwer=YDwr[-1]") %>%
  add_equation("Hs= Hs[-1]*(1+ hsparam*(ph-ph[-1])/ph[-1])") %>%
  add_equation("NHd = NHwd+NHcd+NHgd") %>%
  add_equation("NHcd=Hcd-Hc[-1]") %>%
  add_equation("NHwd=Ms/phc") %>%
  add_equation("NHch=min((Hs-NHgh)*(NHcd/(NHcd+NHwd)),NHcd)") %>%
  add_equation("NHwh=min((Hs-NHgh)*(NHwd/(NHcd+NHwd)),NHwd)") %>%
  add_equation("Md=NHwh*phc") %>%
  add_equation("NHgh = NHgd") %>%
  add_equation("phc=(phparam*(NHcd+NHwd)/(Hs-NHgd)*phc[-1]+phc[-1])") %>%
  add_equation("ph=((NHch+NHwh)/(Hs-Hu))*phc+(NHgh/(Hs-Hu))*phg") %>%
  add_equation("phg=phgparam*phg[-1]") %>%
  add_equation("Hc = NHch+ Hc[-1]") %>%
  add_equation("Hw =NHwh + Hw[-1]") %>%
  add_equation("Hg = NHgh + Hg[-1]") %>%
  add_equation("CGd=(ph-ph[-1])*Hc[-1]") %>%
  add_equation("Hh=NHgh+NHch+NHwh") %>%
  add_equation("Hu=Hs-NHgh-NHch-NHwh") %>%
  add_equation("ERrd=(rentc[-1]+CGd[-1])/(ph[-1]*Hc[-1])") %>%
  add_equation("rh = rb")

# Simulate model
model_lp1 <- simulate_scenario(model_lp1, scenario = "baseline", max_iter = 350, periods = 50, hidden_tol = 0.1, tol = 1e-05, method = "Gauss")

# Create empty shock
shock_lp1 <- create_shock()

# Add shock equation with increased government expenditures and create new scenario with this shock
shock_lp1 <- shock_lp1 %>%
  add_shock(variable = "G", value = 59, desc = "", start = 5, end = 40)

model_lp1 <- model_lp1 %>%
  add_scenario(name = "expansion", origin = "baseline", origin_start=1, origin_end=50, shock = shock_lp1)

# Simulate shock
model_lp1 <- simulate_scenario(model_lp1,
  scenario = "expansion", max_iter = 350, periods = 50,
  hidden_tol = 0.1, tol = 1e-05, method = "Gauss"
)

# Plot results
plot_simulation(model = model_lp1, scenario = c("baseline", "expansion"), from = 1, to = 50, expressions = c("Y", "P"))

# Create sensitivity scenarios for alpha1
model_sen <- create_sensitivity(model_lp1, variable = "alpha1", lower = 0.3, upper = 0.8, step = 0.1)

# Simulate sensitivity for alpha1
model_sen <- simulate_scenario(model_sen, max_iter = 350, periods = 50, hidden_tol = 0.1, tol = 1e-05, method = "Gauss")

# plot sensitivity results for alpha1
plot_simulation(model = model_sen, scenario = "sensitivity", take_all = T, from = 1, to = 50, expressions = c("Y"))

