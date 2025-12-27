library(godley)
library(dplyr)
library(purrr)
library(plotly)
library(tidyverse)

# Plot results
do_plot <- function(m, variables, plot = NULL) {
  variables <- c("time", variables)
  m1 <- m %>%
    mutate(
      uk = Y / K,
      Bsk = Bs / K,
      VK = V / K,
      wi = (W - lag(W)) / lag(W),
      DefY = PSBR/Y,
      GDY = GD/Y,
      LIN = Lfd / IN,
      GRy = (Yk / lag(Yk)) - 1,
      LhYDr = Lhd / YDr,
      FUfInv = FUf / INV,
      EqFMA = (Pe * Ekd) / Vfma,
      GRFfk = -1 + (Ff / P)/(lag(Ff)/lag(P)),
      GRPek = -1 + (Pe / P) / (lag(Pe)/lag(P))) %>%
    pivot_longer(cols = c(-time))
  
  if (is.null(plot)) {
    m2 <- filter(m1, name %in% c("time", variables))
  }
  else {
    m2 <- filter(m1, name %in% c("time", plot))
  }
  print(m2)
  
  m2 %>%
    ggplot(aes(x = time, y = value)) +
    geom_line(aes(linetype = name))
}

do_cbind <- function(m, scenario, variables) {
  variables <- c(variables, "time")
  merge(
    m[[scenario]][["result"]],
    select(m[["baseline"]][["result"]], c(!!variables)) %>% set_names(paste0(names(.), "_bl")),
    by.x = "time", by.y = "time_bl"
  )
}

do_cplot <- function(m, scenario, variables, plot = NULL, from, to) {
  vars = paste0(variables, "_bl")
  ntbl <- do_cbind(m, scenario, variables)
  for (.v in seq_along(variables)) {
    ntbl[, variables[[.v]]] <- ntbl[, variables[[.v]]] / ntbl[, vars[[.v]]]
  }
  # variables <- c(time_col, variables)
  # select(ntbl, c(!!variables))
  
  ntbl %>% 
    filter(time >= from & time <= to) %>%
    do_plot(variables=variables, plot=plot)
}
