library(godley)
library(dplyr)
library(purrr)
library(plotly)
library(tidyverse)

# Plot results
do_plot <- function(m, scenario, variables, t0=1, start = NULL, end = NULL) {

  m1 <- m[[scenario]][["result"]]
  
  start <- if (is.null(start)) t0 else start
  end   <- if (is.null(end)) max(m1[["time"]]) else end
  
  # The lookup_names table has the code of the variables
  # and the name I wanted displayed in the Figures.
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
  
  m1 %>%
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
    mutate(Var = if_else(is.na(Var), name, Var)) %>%
    ggplot(aes(x = time, y = value)) +
    geom_line(aes(color = Var)) +
    scale_color_brewer("Variable", type = 'qual', palette = "Dark2")
}
