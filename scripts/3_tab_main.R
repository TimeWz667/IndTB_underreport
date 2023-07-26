library(tidyverse)



load(here::here("docs", "tabs", "post_main.rdata"))


post01 <- post %>% 
  filter(
    Scenario == "Drug sales data alone" | Scenario == "Drug sales + prevalence survey data"
  ) %>% 
  mutate(
    Scenario = ifelse(Scenario == "Drug sales data alone", "Drug sales data only", "Drug sales with TBPS"),
    Scenario = factor(Scenario, levels = c("Prior distribution", "Drug sales data only", "Drug sales with TBPS"))
  )


post01 %>% 
  mutate(
    dur_pri = dur_pri * 12,
    tp_pri_txi = tp_pri_txi  * 1e-6,
    fp_pri_txi = tp_pri_txi * (1 - ppv_pri) / ppv_pri,
    under = 1 - ppm
  ) %>% 
  select(Scenario, dur_pri, ppv_pri, ppm, tp_pri_txi, fp_pri_txi, under) %>% 
  pivot_longer(-Scenario) %>% 
  group_by(Scenario, name) %>% 
  summarise(
    M = median(value),
    L = quantile(value, 0.025),
    U = quantile(value, 0.975),
    MLU = paste0(round(M, 2), " (", round(L, 2), ", ", round(U, 2), ")")
  )





load(here::here("docs", "tabs", "post_subnational.rdata"))


states <- post  %>% 
  mutate(
    dur_pri = dur_pri * 12,
    n_tp_pri_txi = tp_pri_txi,
    tp_pri_txi = tp_pri_txi * 1e5 / Pop,
    fp_pri_txi = tp_pri_txi * (1 - ppv_pri) / ppv_pri,
    priunder = 1 - ppm
  ) %>% 
  select(State, dur_pri, ppv_pri, ppm, priunder, fp_pri_txi, tp_pri_txi, n_tp_pri_txi) %>% 
  pivot_longer(-State) %>% 
  group_by(State, name) %>% 
  summarise(
    M = median(value),
    L = quantile(value, 0.025),
    U = quantile(value, 0.975),
    MLU = paste0(round(M, 2), " (", round(L, 2), ", ", round(U, 2), ")")
  ) %>% 
  arrange(name)

states %>% 
  filter(name == "n_tp_pri_txi") %>% 
  arrange(M)


states %>% 
  filter(name == "tp_pri_txi")

states %>% 
  filter(name == "fp_pri_txi")


states %>% 
  filter(name == "priunder")
  

