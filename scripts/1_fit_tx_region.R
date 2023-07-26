library(tidyverse)
library(rstan)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)


## Data loading

pop <- local({
  locs <- dir("data")
  locs <- locs[startsWith(locs, "targets_Region")]
  locs <- gsub("targets_Region_", "", locs)
  locs <- gsub(".csv", "", locs)
  locs
  
  state_map <- read_csv(here::here("data", "StateMap.csv")) %>% 
    mutate(
      State = gsub("\\s+", "_", State_Pop),
      State = gsub("&", "and", State)
    )
  
  pop <- read_csv(here::here("data", "population.csv"))
  pop <- pop %>% 
    filter(Year == 2019) %>% 
    filter(Sex == "Total") %>% 
    mutate(
      Location = gsub("\\s+", "_", Location),
      Location = gsub("&", "and", Location)
    ) %>% 
    rename(State = Location) %>% 
    left_join(state_map %>% select(State = State, Region)) %>% 
    group_by(Region) %>% 
    summarise(Pop = sum(Pop)) %>% 
    filter(!is.na(Region))
  
  write_csv(pop, here::here("data", "population_region.csv"))

  pop <- set_names(pop$Pop, pop$Region)[locs]
  
  pop
})



data_locs <- lapply(names(pop), function(loc) {
  
  targets <- read_csv(here::here("data", "targets_Region_" + glue::as_glue(loc) + ".csv"))
  
  drug <- targets %>% filter(Index == "DrugTime") %>% 
    filter(Year == 2019)
  
  tx <- targets %>% filter(Index == "PrTxiPub") %>% 
    mutate(X = round(M * N))
  
  det <- targets %>% 
    filter(Year == 2019) %>% 
    filter(Index == "TxI") %>% 
    mutate(N_Txi = round(N * M))
  
  
  ds <- list(
    N_Txi_Pub = det$N_Txi[1],
    N_Txi_Eng = det$N_Txi[2],
    Pop = unname(pop[loc]),
    Tx = tx$N,
    Tx_Pub = tx$X,
    Drug = drug$M,
    Drug_Std = drug$Error,
    ppv_pub = 0.75,
    dur_upper = 1
  )
  
})
names(data_locs) <- names(pop)



### Fitting ----

src_model <- "tx_11.stan"
model <- rstan::stan_model(here::here("stan", src_model))

folder <- "tx_11_region"
dir.create(here::here("out", folder), showWarnings = F)

for(loc in names(data_locs)) {
  print(loc)
  
  ds <- data_locs[[loc]]
  
  
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
  
  write_csv(tab, file = here::here("out", folder, "summary_" + glue::as_glue(loc) + ".csv"))
  write_csv(ext, file = here::here("out", folder, "post_" + glue::as_glue(loc) + ".csv"))
}

