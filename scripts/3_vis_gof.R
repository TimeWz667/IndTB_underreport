library(rstan)




load(here::here("out", "tx_11", "post.rdata"))


rstan::stan_trace(post, pars = c("ppv_pri", "dur_pri", "ppm"))


