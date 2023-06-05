if ("rstan" %in% (.packages())) {
  detach("package:rstan", unload = T)
}
library(tidyverse)


# Main analysis

post <- bind_rows(lapply(c("tx_01", "tx_11"), function(folder) {
  read_csv(here::here("out", folder, "post.csv")) %>% 
    select(ppm, dur_pri, ppv_pri, p_pri_on_pub, p_pub, 
           tp_pri_drug, tp_pri_drug_time, tp_pri_txi, drug_time) %>% 
    mutate(Scenario = folder, Key = sample(1:n())) %>% 
    filter(Key <= 2000)
})) %>% 
  mutate(
    Scenario = case_when(
      Scenario == "tx_01" ~ "Drug sales data alone",
      T ~ "Drug sales + prevalence survey data"
    )
  )


write_csv(post, here::here("docs", "tabs", "post_main.csv"))
save(post, file = here::here("docs", "tabs", "post_main.rdata"))


# Time-series


post <- bind_rows(
  read_csv(here::here("out", "txts_11", "post.csv")) %>% 
    select(ppm, dur_pri, ppv_pri, p_pri_on_pub, p_pub, 
           tp_pri_drug = tp_pri_drug.2, 
           tp_pri_drug_time = tp_pri_drug_time.2, 
           tp_pri_txi = tp_pri_txi.2, 
           drug_time = drug_time.2) %>% 
    mutate(Scenario = "txts_11", Key = sample(1:n())) %>% 
    filter(Key <= 2000),
  read_csv(here::here("out", "tx_11", "post.csv")) %>% 
    select(ppm, dur_pri, ppv_pri, p_pri_on_pub, p_pub, 
           tp_pri_drug, tp_pri_drug_time, tp_pri_txi, drug_time) %>% 
    mutate(Scenario = "tx_11", Key = sample(1:n())) %>% 
    filter(Key <= 2000)
) %>% 
  mutate(
    Scenario = case_when(
      Scenario == "tx_11" ~ "Single year: 2019",
      T ~ "Drug-sale data: 2018-2020"
    )
  )


write_csv(post, here::here("docs", "tabs", "post_ts.csv"))
save(post, file = here::here("docs", "tabs", "post_ts.rdata"))



# Sub-national

locs <- local({
  locs <- dir(here::here("out", "tx_11_subnational"))
  locs <- locs[startsWith(locs, "post_")]
  locs <- gsub("post_", "", locs)
  locs <- gsub(".csv", "", locs)
  tars <- bind_rows(lapply(locs, function(loc) {
    read_csv(here::here("data", "targets_State_" + glue::as_glue(loc) + ".csv")) %>%
      filter(Index == "PrTxiPub")
  })) %>% filter(Std > 0 & N > 5)
  
  locs <- locs[locs %in% (tars %>% pull(State))]
  
  locs
})


pop <- read_csv(here::here("data", "Population.csv")) %>% 
  filter(Sex == "Total" & Year == 2019) %>% 
  select(State = Location, Pop)


post <- bind_rows(lapply(locs, function(loc) {
  read_csv(here::here("out", "tx_11_subnational", "post_" + glue::as_glue(loc) +".csv")) %>% 
    select(ppm, dur_pri, ppv_pri, p_pri_on_pub, p_under,
           tp_pri_drug, tp_pri_drug_time, tp_pri_txi) %>% 
    mutate(State = loc)
}))%>% 
  mutate(
    State = gsub("_", " ", State)
  ) %>% 
  left_join(pop) 


write_csv(post, here::here("docs", "tabs", "post_subnational.csv"))
save(post, file = here::here("docs", "tabs", "post_subnational.rdata"))


# Regional

locs <- local({
  locs <- dir(here::here("out", "tx_11_regional"))
  locs <- locs[startsWith(locs, "post_")]
  locs <- gsub("post_", "", locs)
  locs <- gsub(".csv", "", locs)
  tars <- bind_rows(lapply(locs, function(loc) {
    read_csv(here::here("data", "targets_Region_" + glue::as_glue(loc) + ".csv")) %>%
      filter(Index == "PrTxiPub")
  })) %>% filter(Std > 0 & N > 5)
  
  locs <- locs[locs %in% (tars %>% pull(Region))]
  
  locs
})


pop <- read_csv(here::here("data", "Population_region.csv"))


post <- bind_rows(lapply(locs, function(loc) {
  read_csv(here::here("out", "tx_11_regional", "post_" + glue::as_glue(loc) +".csv")) %>% 
    select(ppm, dur_pri, ppv_pri, p_pri_on_pub, p_under,
           tp_pri_drug, tp_pri_drug_time, tp_pri_txi) %>% 
    mutate(Region = loc)
}))%>% 
  mutate(
    Region = gsub("_", " ", Region)
  ) %>% 
  left_join(pop) 


write_csv(post, here::here("docs", "tabs", "post_regional.csv"))
save(post, file = here::here("docs", "tabs", "post_regional.rdata"))

