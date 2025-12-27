# model Advanced OPEN

# Configuration
if (!require(here)) install.packages("here")
here::i_am("godley/vignettes/examples/16_Advanced_OPEN_FIX.R")
source(here::here("godley/vignettes/examples/dependencies/00_advanced_open.R"))

# Create empty model
model_open <- create_model(name = "SFC Advanced OPEN")

# Add variables
model_open <- model_open |>
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
  
  # Exogenous
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
  
  # Endogenous
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
  
  # Other endogenous
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
  
  # Add variables and equations used in the sfcr model but not explicitly declared
  add_variable("yd_hs_uk") |>
  add_variable("yd_hs_us") |>
  add_variable("kab_uk") |>
  add_variable("kab_us") |>
  add_variable("psbr_uk") |>
  add_variable("psbr_us") |>
  add_variable("nafa_uk") |>
  add_variable("nafa_us") |>
  add_variable("nwcb_uk")

# Add equations
model_open <- model_open |>
  # Disposable income in UK - eq. 12.1
  add_equation("yd_uk = (y_uk + r_uk[-1]*b_ukuk_d[-1] + xr_us*r_us[-1]*b_ukus_s[-1])*(1 - theta_uk) + (xr_us - xr_us[-1])*b_ukus_s[-1]", desc = "") |>
  
  # Haig-Simons disposable income in UK - eq. 12.2
  add_equation("yd_hs_uk = yd_uk + (xr_us - xr_us[-1])*b_ukus_s[-1]", desc = "") |>
  
  # Wealth accumulation in UK - eq. 12.3
  add_equation("v_uk = v_uk[-1] + yd_uk - cons_uk", desc = "") |>
  
  # Disposable income in US - eq. 12.4
  add_equation("yd_us = (y_us + r_us[-1]*b_usus_d[-1] + xr_uk*r_uk[-1]*b_usuk_s[-1])*(1 - theta_us) + (xr_uk - xr_uk[-1])*b_usuk_s[-1]", desc = "") |>
  
  # Haig-Simons disposable income in US - eq. 12.5
  add_equation("yd_hs_us = yd_us + d(xr_uk)*b_usuk_s[-1]", desc = "") |>
  
  # Wealth accumulation in US - eq. 12.6
  add_equation("v_us = v_us[-1] + yd_us - cons_us", desc = "") |>
  
  # Taxes in UK - eq. 12.7
  add_equation("t_uk = theta_uk*(y_uk + r_uk[-1]*b_ukuk_d[-1] + xr_us*r_us[-1]*b_ukus_s[-1])", desc = "") |>
  
  # Taxes in US - eq. 12.8
  add_equation("t_us = theta_us*(y_us + r_us[-1]*b_usus_d[-1] + xr_uk*r_uk[-1]*b_usuk_s[-1])", desc = "") |>
  
  # Equations 12.9 & 12.10 are dropped in favour of equations 12.53 & 12.54
  # Profits of Central Bank in UK - eq. 12.11 - typo in the book for r_us
  add_equation("f_cb_uk = r_uk[-1]*b_cb_ukuk_d[-1] + r_us[-1]*b_cb_ukus_s[-1]*xr_us", desc = "") |>
  
  # Profits of Central Bank in US - eq. 12.12
  add_equation("f_cb_us = r_us[-1]*b_cb_usus_d[-1]", desc = "") |>
  
  # Government budget constraint - UK - eq. 12.13
  add_equation("b_uk_s = b_uk_s[-1] + g_uk + r_uk[-1]*b_uk_s[-1] - t_uk - f_cb_uk", desc = "") |>
  
  # Government budget constraint - US - eq. 12.14
  add_equation("b_us_s = b_us_s[-1] + g_us + r_us[-1]*b_us_s[-1] - t_us - f_cb_us", desc = "") |>
  
  # Current account balance - UK - eq. 12.15
  add_equation("cab_uk = x_uk - im_uk + xr_us*r_us[-1]*b_ukus_s[-1] - r_uk[-1]*b_usuk_s[-1] + r_us[-1]*b_cb_ukus_s[-1]*xr_us", desc = "") |>
  
  # Capital account balance in UK - eq. 12.16
  add_equation("kab_uk = kabp_uk - (xr_us*(b_cb_ukus_s - b_cb_ukus_s[-1]) + pg_uk*(or_uk - or_uk[-1]))", desc = "") |>
  
  # Current account balance in US - eq. 12.17
  add_equation("cab_us = x_us - im_us + xr_uk*r_uk[-1]*b_usuk_s[-1] - r_us[-1]*b_ukus_s[-1] - r_us[-1]*b_cb_ukus_s[-1]", desc = "") |>
  
  # Capital account balance in US - eq. 12.18
  add_equation("kab_us = kabp_us + (b_cb_ukus_s - b_cb_ukus_s[-1]) - pg_us*(or_us - or_us[-1])", desc = "") |>
  
  # Capital account balance in UK, net of official transactions - eq. 12.19
  add_equation("kabp_uk = -(b_ukus_s - b_ukus_s[-1])*xr_us + (b_usuk_s - b_usuk_s[-1])", desc = "") |>
  
  # Capital account balance in US, net of official transactions - eq. 12.20
  add_equation("kabp_us = -(b_usuk_s - b_usuk_s[-1])*xr_uk + (b_ukus_s - b_ukus_s[-1])", desc = "") |>
  
  # TRADE
  # Import prices in UK - eq. 12.21
  add_equation("pm_uk = exp(nu0m + nu1m*log(py_us) + (1 - nu1m)*log(py_uk) - nu1m*log(xr_uk))", desc = "") |>
  
  # Export prices in UK - eq. 12.22
  add_equation("px_uk = exp(nu0x + nu1x*log(py_us) + (1 - nu1x)*log(py_uk) - nu1x*log(xr_uk))", desc = "") |>
  
  # Export prices in US - eq. 12.23
  add_equation("px_us = pm_uk*xr_uk", desc = "") |>
  
  # Import prices in US - eq. 12.24
  add_equation("pm_us = px_uk*xr_uk", desc = "") |>
  
  # Real exports from UK - eq. 12.25 - depends on current relative price
  add_equation("x_k_uk = exp(eps0 - eps1*log(pm_us/py_us) + eps2*log(y_k_us))", desc = "") |>
  
  # Real imports of UK - eq. 12.26
  add_equation("im_k_uk = exp(mu0 - mu1*log(pm_uk[-1]/py_uk[-1]) + mu2*log(y_k_uk))", desc = "") |>
  
  # Real exports from US - eq. 12.27
  add_equation("x_k_us = im_k_uk", desc = "") |>
  
  # Real imports of US - eq. 12.28
  add_equation("im_k_us = x_k_uk", desc = "") |>
  
  # Exports of UK - eq. 12.29
  add_equation("x_uk = x_k_uk*px_uk", desc = "") |>
  
  # Exports of US - eq. 12.30
  add_equation("x_us = x_k_us*px_us", desc = "") |>
  
  # Imports of UK - eq. 12.31
  add_equation("im_uk = im_k_uk*pm_uk", desc = "") |>
  
  # Imports of US - eq. 12.32
  add_equation("im_us = im_k_us*pm_us", desc = "") |>
  
  # INCOME AND EXPENDITURE
  # Real wealth in UK - eq. 12.33
  add_equation("v_k_uk = v_uk/pds_uk", desc = "") |>
  
  # Real wealth in US - eq. 12.34
  add_equation("v_k_us = v_us/pds_us", desc = "") |>
  
  # Real Haig-Simons disposable income in UK - eq. 12.35
  add_equation("ydhs_k_uk = yd_uk/pds_uk - v_k_uk[-1]*(pds_uk - pds_uk[-1])/pds_uk", desc = "") |>
  
  # Real Haig-Simons disposable income in US - eq. 12.36
  add_equation("ydhs_k_us = yd_us/pds_us - v_k_us[-1]*(pds_us - pds_us[-1])/pds_us", desc = "") |>
  
  # Real consumption in UK - eq. 12.37
  add_equation("c_k_uk = alpha1_uk*ydhse_k_uk + alpha2_uk*v_k_uk[-1]", desc = "") |>
  
  # Real consumption in US - eq. 12.38
  add_equation("c_k_us = alpha1_us*ydhse_k_us + alpha2_us*v_k_us[-1]", desc = "") |>
  
  # Expected real Haig-Simons disposable income in UK - eq. 12.39
  add_equation("ydhse_k_uk = (ydhs_k_uk + ydhs_k_uk[-1])/2", desc = "") |>
  
  # Expected real Haig-Simons disposable income in US - eq. 12.40
  add_equation("ydhse_k_us = (ydhs_k_us + ydhs_k_us[-1])/2", desc = "") |>
  
  # Real sales in UK - eq. 12.41
  add_equation("s_k_uk = c_k_uk + g_k_uk + x_k_uk", desc = "") |>
  
  # Real sales in US - eq. 12.42
  add_equation("s_k_us = c_k_us + g_k_us + x_k_us", desc = "") |>
  
  # Value of sales in UK - eq. 12.43
  add_equation("s_uk = s_k_uk*ps_uk", desc = "") |>
  
  # Value of sales in US - eq. 12.44
  add_equation("s_us = s_k_us*ps_us", desc = "") |>
  
  # Price of sales in UK - eq. 12.45
  add_equation("ps_uk = (1 + phi_uk)*(w_uk*n_uk + im_uk)/s_k_uk", desc = "") |>
  
  # Price of sales in US - eq. 12.46
  add_equation("ps_us = (1 + phi_us)*(w_us*n_us + im_us)/s_k_us", desc = "") |>
  
  # Price of domestic sales in UK - eq. 12.47
  add_equation("pds_uk = (s_uk - x_uk)/(s_k_uk - x_k_uk)", desc = "") |>
  
  # Price of domestic sales in US - eq. 12.48
  add_equation("pds_us = (s_us - x_us)/(s_k_us - x_k_us)", desc = "") |>
  
  # Domestic sales in UK - eq. 12.49
  add_equation("ds_uk = s_uk - x_uk", desc = "") |>
  
  # Domestic sales in US - eq. 12.50
  add_equation("ds_us = s_us - x_us", desc = "") |>
  
  # Real domestic sales in UK - eq. 12.51
  add_equation("ds_k_uk = c_k_uk + g_k_uk", desc = "") |>
  
  # Real domestic sales in US - eq. 12.52
  add_equation("ds_k_us = c_k_us + g_k_us", desc = "") |>
  
  # Value of output in UK - eq. 12.53
  add_equation("y_uk = s_uk - im_uk", desc = "") |>
  
  # Value of output in US - eq. 12.54
  add_equation("y_us = s_us - im_us", desc = "") |>
  
  # Value of real output in UK - eq. 12.55
  add_equation("y_k_uk = s_k_uk - im_k_uk", desc = "") |>
  
  # Value of real output in US - eq. 12.56
  add_equation("y_k_us = s_k_us - im_k_us", desc = "") |>
  
  # Price of output in UK - eq. 12.57
  add_equation("py_uk = y_uk/y_k_uk", desc = "") |>
  
  # Price of output in US - eq. 12.58
  add_equation("py_us = y_us/y_k_us", desc = "") |>
  
  # Consumption in UK - eq. 12.59
  add_equation("cons_uk = c_k_uk*pds_uk", desc = "") |>
  
  # Consumption in US - eq. 12.60
  add_equation("cons_us = c_k_us*pds_us", desc = "") |>
  
  # Government expenditure in UK - eq. 12.61
  add_equation("g_uk = g_k_uk*pds_uk", desc = "") |>
  
  # Government expenditure in US - eq. 12.62
  add_equation("g_us = g_k_us*pds_us", desc = "") |>
  
  # Note: tax definitions in the book as eqns 12.63 & 12.64 are already as eqns 12.7 & 12.8
  # Employment in UK - eq. 12.65
  add_equation("n_uk = y_k_uk/pr_uk", desc = "") |>
  
  # Employment in US - eq. 12.66
  add_equation("n_us = y_k_us/pr_us", desc = "") |>
  
  # ASSET DEMANDS
  # Demand for UK bills in UK - eq. 12.67
  add_equation("b_ukuk_d = v_uk*(lambda10 + lambda11*r_uk - lambda12*(r_us + dxre_us))", desc = "") |>
  
  # Demand for US bills in UK - 12.68F
  add_equation("b_ukus_d = v_uk*(lambda20 - lambda21*r_uk + lambda22*(r_us + dxre_us))", desc = "") |>
  
  # Base interest rates r_uk - eq. 12.68R
  # r_uk = (lambda20 + lambda22*(r_us + dxre_us) - b_ukus_d/v_uk)/lambda21"
  
  # Demand for money in UK - eq. 12.69
  add_equation("h_uk_d = v_uk - b_ukuk_d - b_ukus_d", desc = "") |>
  
  # Demand for US	bills in US - eq. 12.70
  add_equation("b_usus_d = v_us*(lambda40 + lambda41*r_us - lambda42*(r_uk + dxre_uk))", desc = "") |>
  
  # Demand for UK bills in US - eq. 12.71
  add_equation("b_usuk_d = v_us*(lambda50 - lambda51*r_us + lambda52*(r_uk + dxre_uk))", desc = "") |>
  
  # Demand for money in US - eq. 12.72
  add_equation("h_us_d = v_us - b_usus_d - b_usuk_d", desc = "") |>
  
  # Note - we follow eqns numbering in the text...

  # Expected change in UK exchange rate - eq. 12.75
  # dxre_uk = (xre_uk - xr_uk[-1])/xr_uk
  
  # Expected change in US exchange rate - eq. 12.76
  # dxre_us = (xre_us - xr_us[-1])/xr_us
  
  # ASSET SUPPLIES
  # Suply of cash in US - eq. 12.77
  add_equation("h_us_s = h_us_d", desc = "") |>
  
  # Supply of US bills to CountryN - eq. 12.78
  add_equation("b_usus_s = b_usus_d", desc = "") |>
  
  # Supply of US bills to US Central bank - eq. 12.79
  add_equation("b_cb_usus_s = b_cb_usus_d", desc = "") |>
  
  # Suply of cash in UK - eq. 12.80
  add_equation("h_uk_s = h_uk_d", desc = "") |>
  
  # Bills issued by US acquired by US - eq. 12.81
  add_equation("b_ukuk_s = b_ukuk_d", desc = "") |>
  
  # Supply of UK bills to UK Central bank - eq. 12.82
  # MODLER MACRO VERSION
  add_equation("b_cb_ukuk_s = b_cb_ukuk_d", desc = "") |>
  # BOOK VERSION - eq. 12.82A
  # b_cb_ukuk_s = b_uk_s - b_ukuk_s - b_usuk_s
  
  # Balance sheet of US Central bank - eq. 12.83 - expressed as changes
  add_equation("b_cb_usus_d = b_cb_usus_d[-1] + (h_us_s - h_us_s[-1]) - (or_us - or_us[-1])*pg_us", desc = "") |>
  
  # Balance sheet of UK Central bank - eq. 12.84
  add_equation("b_cb_ukuk_d = b_cb_ukuk_d[-1] + (h_uk_s - h_uk_s[-1]) - (b_cb_ukus_s - b_cb_ukus_s[-1])*xr_us - (or_uk - or_uk[-1])*pg_uk", desc = "") |>
  
  # Price of gold is equal in the two countries - eq. 12.85
  add_equation("pg_uk = pg_us/xr_uk", desc = "") |>
  
  # US exchange rate - eq. 12.86
  add_equation("xr_us = 1/xr_uk", desc = "") |>
  
  # Equilibrium condition for bills issued by UK acquired by US - eq. 12.87
  add_equation("b_usuk_s = b_usuk_d*xr_us", desc = "") |>
  
  # Equilibrium condition for bills issued by US acquired by UK Central bank - eq. 12.88
  add_equation("b_cb_ukus_d = b_cb_ukus_s*xr_us", desc = "") |>
  
  # UK Exchange rate - eq. 12.89FL - xr_uk is now exogenous
  # xr_uk = b_ukus_s/b_ukus_d
  
  # Government deficit in the UK
  add_equation("psbr_uk = g_uk + r_uk[-1]*b_uk_s[-1] - t_uk - f_cb_uk", desc = "") |>
  
  # Government deficit in the US
  add_equation("psbr_us = g_us + r_us[-1]*b_us_s[-1] - t_us - f_cb_us", desc = "") |>
  
  # Net accumulation of financial assets in the UK
  add_equation("nafa_uk = psbr_uk + cab_uk", desc = "") |>
  
  # Net accumulation of financial assets in the US
  add_equation("nafa_us = psbr_us + cab_us", desc = "") |>
  
  # Hidden equation
  add_equation("b_cb_ukuk_sa = b_uk_s - b_ukuk_s - b_usuk_s", desc = "") |>
  
  # Net wealth of CBUK
  add_equation("nwcb_uk = -h_uk_d + b_cb_ukuk_d + b_cb_ukus_d * xr_us + or_uk * pg_uk", desc = "") |>
  
  # Model OPEN FIX Closure
  # 12.89R : Demand of UK Bills in US
  add_equation("b_ukus_s = xr_uk*b_ukus_d", desc = "") |>
  # 12.90R : Supply of UK bills to us
  add_equation("b_cb_ukus_s = b_us_s - b_usus_s - b_cb_usus_d - b_ukus_s", desc = "") |>
  
  # Hidden equation
  add_equation("b_cb_ukuk_s = b_cb_ukuk_sa", hidden = TRUE)

# Simulate model
model_open <- simulate_scenario(model_open, scenario = "baseline",
                                  max_iter = 350, periods = 100, tol = 1e-15,
                                  hidden_tol = 0.1, method = "Broyden")
# Structure of the model (DAG)
plot_cycles(model_open)

# Scenario 1: Increase in the US propensity to import
# Create empty shock and add shock equation
shock_open <- create_shock() |>
  add_shock(variable = "eps0", value = -2, start = 5, end = 100, desc = "")

model_open <- model_open |>
  add_scenario(name = "eps0_shock", origin = "baseline", shock = shock_open)

# Simulate shock
model_open <- simulate_scenario(model_open, scenario = "eps0_shock", periods = 100,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
# Figure 12.2A
do_plot(m = model_open, scenario = "eps0_shock",
        variables = c("cab_uk", "nafa_uk", "gab_uk", "tab_uk"),
        t0 = 1, end = 50)

# Figure 12.1B
do_plot(m = model_open, scenario = "eps0_shock",
        variables = c("cab_uk", "dres_uk", "db_cb_uk", "dh_uk"),
        t0 = 1, end = 50)

# Figure 12.1C
do_plot(m = model_open, scenario = "eps0_shock",
        variables = c("by_uk", "by_us"),
        t0 = 1, end = 50)

# Scenario 2: A one-step devaluation after an increase in the UK propensity to import
# Create empty shock and add shock equation
shock_open <- create_shock() |>
  add_shock(variable = "mu0", value = -2, start = 5, end = 100, desc = "") |>
  add_shock(variable = "xr_uk", value = 0.84, start = 10, end = 100, desc = "")

model_open <- model_open |>
  add_scenario(name = "mu0-xr_uk_shock", origin = "baseline", shock = shock_open)

# Simulate shock
model_open <- simulate_scenario(model_open, scenario = "mu0-xr_uk_shock", periods = 100,
                                  max_iter = 350, tol = 1e-10, method = "Broyden",
                                  hidden_tol = 0.1, rhtol = TRUE)
# Plot results
# Figure 12.4A
do_plot(m = model_open, scenario = "mu0-xr_uk_shock",
        variables = c("tab_uk", "cab_uk"),
        t0 = 1, end = 50)

# Figure 12.4B
do_plot(m = model_open, scenario = "mu0-xr_uk_shock",
        variables = c("y_k_uk", "xr_uk"),
        t0 = 1, end = 50) +
  facet_wrap(~name, scales = "free")

