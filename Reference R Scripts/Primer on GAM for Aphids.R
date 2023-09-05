library(mgcv)
library(ecodatamisc)

dat <- ld_dat_cdd

gam(cumu_dd ~ s(julian_day), data=dat) %>% ecodatamisc::plot_gam()
