library(tidyverse)
library(tidybayes)




dd <- read_csv(here::here("out", "cs_11", "post.csv"))


read_csv(here::here("out", "cs_11", "post.csv")) %>% 
  select(tp_pri_drug_time, tp_pri_drug, tp_pri_txi, p_under) %>% 
  mutate(Case = "CS11")



dd <- bind_rows(
  read_csv(here::here("out", "cs_free", "post.csv")) %>% 
    select(tp_pri_drug_time, tp_pri_drug, tp_pri_txi, p_under) %>% 
    mutate(Case = "CSF"),
  read_csv(here::here("out", "cs_exclusive", "post.csv")) %>% 
    select(tp_pri_drug_time, tp_pri_drug, tp_pri_txi, p_under) %>% 
    mutate(Case = "CSE"),
  read_csv(here::here("out", "cs_independent", "post.csv")) %>% 
    select(tp_pri_drug_time, tp_pri_drug, tp_pri_txi, p_under) %>% 
    mutate(Case = "CSI"),
  read_csv(here::here("out", "tx_11", "post.csv")) %>% 
    select(tp_pri_drug_time, tp_pri_drug, tp_pri_txi, p_under) %>% 
    mutate(Case = "TX11")
)



dd %>% 
  ggplot() +
  stat_halfeye(aes(x = p_under, y = Case), alpha = 0.3)


dd %>% 
  select(Case, starts_with("tp")) %>% 
  pivot_longer(-Case) %>% 
  ggplot() +
  stat_halfeye(aes(x = value, y = Case), alpha = 0.3) +
  facet_grid(.~name) +
  expand_limits(x = 0)



d %>% select(starts_with("ppv_")) %>% 
  ggplot() +
  geom_point(aes(x = ppv_pub, y = ppv_eng)) +
  geom_abline(slope = 1)







