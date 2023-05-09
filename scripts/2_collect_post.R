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


# Sub-national

locs <- local({
  locs <- dir(here::here("out", "tx_11_subnational"))
  locs <- locs[startsWith(locs, "post_")]
  locs <- gsub("post_", "", locs)
  locs <- gsub(".csv", "", locs)
  tars <- bind_rows(lapply(locs, function(loc) {
    read_csv(here::here("data", "targets_" + glue::as_glue(loc) + ".csv")) %>%
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



# Sens, PPM


ds <- dir(here::here("out", "sens_ppm_lo"))
ds <- ds[startsWith(ds, "post_")]


post <- bind_rows(lapply(ds, function(file) {
  read_csv(here::here("out", "sens_ppm_lo", file)) %>% 
    select(ppm, dur_pri, ppv_pri, p_pri_on_pub, 
           tp_pri_drug, tp_pri_drug_time, tp_pri_txi) %>% 
    mutate(Scenario = file)
})) %>% 
  extract(Scenario, "PPM_lb", "post_(\\S+).csv", convert=T)

write_csv(post, here::here("docs", "tabs", "post_ppm_lo.csv"))
save(post, file = here::here("docs", "tabs", "post_ppm_lo.rdata"))



ds <- dir(here::here("out", "sens_ppm_shape"))
ds <- ds[startsWith(ds, "post_")]


post <- bind_rows(lapply(ds, function(file) {
  read_csv(here::here("out", "sens_ppm_shape", file)) %>% 
    select(ppm, dur_pri, ppv_pri, p_pri_on_pub, 
           tp_pri_drug, tp_pri_drug_time, tp_pri_txi) %>% 
    mutate(Scenario = file)
})) %>% 
  extract(Scenario, "PPM_avg", "post_(\\S+).csv", convert=T)


write_csv(post, here::here("docs", "tabs", "post_ppm_shape.csv"))
save(post, file = here::here("docs", "tabs", "post_ppm_shape.rdata"))



# Sens, incremental structures

post <- bind_rows(lapply(c("tx_11", "dx_11", "cs_11"), function(folder) {
  read_csv(here::here("out", folder, "post.csv")) %>% 
    select(ppm, dur_pri, ppv_pri, p_pri_on_pub, p_under,
           tp_pri_drug, tp_pri_drug_time, tp_pri_txi) %>% 
    mutate(Scenario = folder)
})) %>% 
  mutate(
    Scenario = factor(Scenario, c("tx_11", "dx_11", "cs_11"))
  )

write_csv(post, here::here("docs", "tabs", "post_incre.csv"))
save(post, file = here::here("docs", "tabs", "post_incre.rdata"))

