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
  
  m1 %>%
    mutate(tab_uk = x_uk - im_uk,
           tab_us = x_us - im_us,
           gab_uk = -psbr_uk,
           gab_us = -psbr_us,
           dres_uk = b_cb_ukus_d - lag(b_cb_ukus_d),
           db_cb_uk = b_cb_ukuk_d - lag(b_cb_ukuk_d),
           dh_uk = h_uk_s - lag(h_uk_s),
           dh_us = h_us_s - lag(h_us_s),
           by_uk = b_uk_s / y_uk,
           by_us = b_us_s / y_us,
           bukus_p = (b_ukus_d/xr_us)/v_uk,
           bukus_d = b_ukus_d / v_uk) %>%
    filter(time >= start & time <= end) %>%
    pivot_longer(cols = -time) %>%
    filter(name %in% variables) %>%
    ggplot(aes(x = time, y = value)) +
    geom_line(aes(linetype = name))
}
