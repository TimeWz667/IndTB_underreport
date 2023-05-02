library(tidyverse)
library(tidybayes)

theme_set(theme_bw() + theme(text = element_text(family = "sans")))


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


locs


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

pop


stats <- post %>% 
  pivot_longer(-c(State, Pop), names_to = "Index") %>% 
  group_by(State, Pop, Index) %>% 
  summarise(
    M = median(value),
    L1 = quantile(value, 0.025),
    U1 = quantile(value, 0.975),
    L2 = quantile(value, 0.25),
    U2 = quantile(value, 0.75),
    U = U1,
    L = L1
  ) %>% 
  ungroup()


post %>% 
  mutate(
    State = reorder(State, ppm)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = ppm, y = State)) +
  expand_limits(x = c(0, 1))


post %>% 
  mutate(
    State = reorder(State, dur_pri)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = dur_pri, y = State)) +
  expand_limits(x = c(0, 2))


post %>% 
  mutate(
    State = reorder(State, p_under)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = p_under, y = State)) +
  expand_limits(x = c(0, 1))


stats %>% 
  filter(Index == "p_under") %>% 
  mutate(
    State = reorder(State, M)
  ) %>% 
  ggplot() +
  geom_pointinterval(aes(x = M, xmin = L1, xmax = U1, y = State)) +
  geom_errorbar(aes(x = M, xmin = L2, xmax = U2, y = State), linewidth = rel(0.6), width = 0.6) +
  geom_errorbar(aes(x = M, xmin = L1, xmax = U1, y = State), linewidth = rel(0.3), width = 0.25)



g_dur_pri <- stats %>% 
  filter(Index == "dur_pri") %>% 
  mutate(
    State = reorder(State, M)
  ) %>% 
  ggplot() +
  geom_pointinterval(aes(x = M, xmin = L1, xmax = U1, y = State)) + 
  scale_x_continuous("Treatment duration, on private drug, month", labels = scales::number_format(scale = 12)) +
  expand_limits(x = 0)


g_dur_pri


g_ppv_pri <- stats %>% 
  filter(Index == "ppv_pri") %>% 
  mutate(
    State = reorder(State, M)
  ) %>% 
  ggplot() +
  geom_pointinterval(aes(x = M, xmin = L1, xmax = U1, y = State)) + 
  scale_x_continuous("Positive Predictive Value, in the private sector, %", labels = scales::percent_format()) + 
  expand_limits(x = c(0, 1))

g_ppv_pri


g_drug <- post %>% 
  mutate(
    State = reorder(State, tp_pri_drug / Pop)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_drug / Pop, y = State)) +
  #geom_pointinterval(aes(x = M / Pop, xmin = L / Pop, xmax = U / Pop, y = State), size = rel(2)) + 
  scale_x_continuous("Treatment initiated with private drugs, per 100,000", 
                     labels=scales::number_format(scale = 1e5), limits = c(0, 2e-3)) + 
  expand_limits(x = 0)

g_drug


g_drugT <- post %>% 
  mutate(
    State = reorder(State, tp_pri_drug_time)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_drug_time, y = State)) +
  # geom_pointinterval(aes(x = M, xmin = L, xmax = U, y = State)) + 
  scale_x_continuous("Caseloads with private drugs, millions", labels=scales::number_format(scale = 1e-6)) + 
  expand_limits(x = c(0, 1))

g_drugT



g_txi <- post %>% 
  mutate(
    State = reorder(State, tp_pri_txi / Pop)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_txi / Pop, y = State)) +
  scale_x_continuous("Unreported private cases, per 100,000", labels=scales::number_format(scale = 1e5), limits = c(0, 2e-3)) + 
  expand_limits(x = 0)

g_txi


g_under <- post %>% 
  mutate(
    State = reorder(State, p_under)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = p_under, y = State)) +
  scale_x_continuous("Unreported private cases / All cases initiated treatment, %", labels=scales::percent) + 
  expand_limits(x = 0:1)

g_under


g_priunder <- post %>% 
  mutate(
    State = reorder(State, 1 - ppm)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = 1 - ppm, y = State)) +
  #geom_pointinterval(aes(x = M, xmin = L2, xmax = U2, y = State)) + 
  scale_x_continuous("Unreported private cases / All private cases initiated treatment, %", labels=scales::percent) + 
  expand_limits(x = 0:1)

g_priunder


ggsave(g_ppv_pri, filename = here::here("docs", "figs", "g_sn_ppv.png"), width = 6, height = 5)
ggsave(g_dur_pri, filename = here::here("docs", "figs", "g_sn_dur.png"), width = 6, height = 5)
ggsave(g_drug, filename = here::here("docs", "figs", "g_sn_drug.png"), width = 6, height = 5)
ggsave(g_drugT, filename = here::here("docs", "figs", "g_sn_drugt.png"), width = 6, height = 5)
ggsave(g_txi, filename = here::here("docs", "figs", "g_sn_txi.png"), width = 6, height = 5)
ggsave(g_under, filename = here::here("docs", "figs", "g_sn_under.png"), width = 6, height = 5)
ggsave(g_priunder, filename = here::here("docs", "figs", "g_sn_priunder.png"), width = 6, height = 5)



