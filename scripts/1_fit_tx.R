library(tidyverse)
library(rstan)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)


## Data loading

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
  ppv_pub = 0.75,
  dur_upper = 1
)



### Fitting ----

for(src_model in c("tx_00.stan", "tx_10.stan", "tx_01.stan", "tx_11.stan")) {
  model <- rstan::stan_model(here::here("stan", src_model))
  
  post <- rstan::sampling(model, data=ds, iter=2e4, warmup=2e4-1000)
  
  tab <- as.data.frame(summary(post)$summary)
  tab$Name <- rownames(tab)
  tab <- tab %>% as_tibble() %>% relocate(Name)
  
  ext <- rstan::extract(post) %>% 
    data.frame() %>% 
    as.tibble() %>% 
    mutate(
      ppv_pub = ds$ppv_pub,
      ent_pub = ds$ent_pub,
      pdx_pub = ds$pdx_pub
    )
  
  folder <- gsub(".stan", "", src_model)
  dir.create(here::here("out", folder), showWarnings = F)
  write_csv(tab, file = here::here("out", folder, "summary.csv"))
  write_csv(ext, file = here::here("out", folder, "post.csv"))
}




