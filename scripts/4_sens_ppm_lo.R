library(tidyverse)
library(rstan)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)


## Data loading

pr_pub <- jsonlite::read_json(here::here("data", "PrPublicPrior.json"), simplifyVector = T)


targets <- read_csv(here::here("data", "targets_india.csv"))

targets %>% 
  filter(Year == 2019) %>% data.frame()


drug <- targets %>% filter(Index == "DrugTime") %>% 
  filter(Year == 2019)

tx <- targets %>% filter(Index == "PrTxiPub") %>% 
  mutate(X = round(M * N))

det <- targets %>% 
  filter(Year == 2019) %>% 
  filter(Index == "TxI") %>% 
  mutate(
    N_Txi = round(N * M)
  )


ds <- list(
  N_Txi_Pub = det$N_Txi[1],
  N_Txi_Eng = det$N_Txi[2],
  Pop = 1383112050,
  Tx = tx$N,
  Tx_Pub = tx$X,
  Drug = drug$M,
  Drug_Std = drug$Error,
  i_u_al = pr_pub$India$al,
  i_u_be = pr_pub$India$be,
  ppv_pub = 0.75,
  dur_upper = 1
)



### Fitting ----

model <- rstan::stan_model(here::here("stan", "tx_11_ppm.stan"))

folder <- "sens_ppm_lo"
dir.create(here::here("out", folder), showWarnings = F)


for (pl in seq(0, 0.2, 0.025)) {
  ds$ppm_lo <- pl
  
  post <- rstan::sampling(model, data=ds, iter=5e3, warmup=4e3)
  
  tab <- as.data.frame(summary(post)$summary)
  tab$Name <- rownames(tab)
  tab <- tab %>% as_tibble() %>% relocate(Name)
  
  ext <- rstan::extract(post) %>% 
    data.frame() %>% 
    as.tibble() %>% 
    mutate(
      ppv_pub = ds$ppv_pub,
      ent_pub = ds$entpub,
      pdx_pub = ds$pdx_pub
    )
  
  
  write_csv(tab, file = here::here("out", folder, "summary_" + glue::as_glue(sprintf("%.4f", pl)) + ".csv"))
  write_csv(ext, file = here::here("out", folder, "post_" + glue::as_glue(sprintf("%.4f", pl)) + ".csv"))
  
}







