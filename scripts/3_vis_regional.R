if ("rstan" %in% (.packages())) {
  detach("package:rstan", unload = T)
}
library(tidyverse)
library(tidybayes)

theme_set(theme_bw() + theme(text = element_text(family = "sans")))


load(file = here::here("docs", "tabs", "post_regional.rdata"))


stats <- post %>% 
  pivot_longer(-c(Region, Pop), names_to = "Index") %>% 
  group_by(Region, Pop, Index) %>% 
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



stats %>% 
  filter(Index == "p_under") %>% 
  mutate(
    Region = reorder(Region, M)
  ) %>% 
  ggplot() +
  geom_pointinterval(aes(x = M, xmin = L1, xmax = U1, y = Region)) +
  geom_errorbar(aes(x = M, xmin = L2, xmax = U2, y = Region), linewidth = rel(0.6), width = 0.6) +
  geom_errorbar(aes(x = M, xmin = L1, xmax = U1, y = Region), linewidth = rel(0.3), width = 0.25)



g_dur_pri <- stats %>% 
  filter(Index == "dur_pri") %>% 
  mutate(
    Region = reorder(Region, M)
  ) %>% 
  ggplot() +
  geom_pointinterval(aes(x = M, xmin = L1, xmax = U1, y = Region)) + 
  scale_x_continuous("Treatment duration, on private drug, month", labels = scales::number_format(scale = 12)) +
  expand_limits(x = 0)


g_dur_pri


g_ppv_pri <- stats %>% 
  filter(Index == "ppv_pri") %>% 
  mutate(
    Region = reorder(Region, M)
  ) %>% 
  ggplot() +
  geom_pointinterval(aes(x = M, xmin = L1, xmax = U1, y = Region)) + 
  scale_x_continuous("Positive Predictive Value, in the private sector, %", labels = scales::percent_format()) + 
  expand_limits(x = c(0, 1))

g_ppv_pri


g_drug <- post %>% 
  mutate(
    Region = reorder(Region, tp_pri_drug / Pop)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_drug / Pop, y = Region)) +
  #geom_pointinterval(aes(x = M / Pop, xmin = L / Pop, xmax = U / Pop, y = State), size = rel(2)) + 
  scale_x_continuous("Treatment initiated with private drugs, per 100,000", 
                     labels=scales::number_format(scale = 1e5), limits = c(0, 2e-3)) + 
  expand_limits(x = 0)

g_drug


g_drugT <- post %>% 
  mutate(
    Region = reorder(Region, tp_pri_drug_time)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_drug_time, y = Region)) +
  # geom_pointinterval(aes(x = M, xmin = L, xmax = U, y = State)) + 
  scale_x_continuous("Caseloads with private drugs, millions", labels=scales::number_format(scale = 1e-6)) + 
  expand_limits(x = c(0, 1))

g_drugT



g_txi <- post %>% 
  mutate(
    Region = reorder(Region, tp_pri_txi / Pop)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_txi / Pop, y = Region)) +
  scale_x_continuous("Unreported number with TB initiating private treatment, \nper 100,000", labels=scales::number_format(scale = 1e5), limits = c(0, 2e-3)) + 
  expand_limits(x = 0)

g_txi


g_txi_abs <- post %>% 
  mutate(
    Region = reorder(Region, tp_pri_txi)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_txi, y = Region)) +
  scale_x_continuous("Unreported number with TB initiating private treatment, \nthousands", 
                     labels=scales::number_format(scale = 1e-3), limits = c(0, 5e5)) + 
  expand_limits(x = 0)

g_txi_abs


g_txi_labs <- post %>% 
  mutate(
    Region = reorder(Region, tp_pri_txi)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_txi, y = Region)) +
  scale_x_log10("Unreported number with TB initiating private treatment, \n thousands at logarithmic scale", 
                breaks = c(5, 10, 50, 100, 500, 1000) * 1e3,
                labels=scales::number_format(scale = 1e-3)) + 
  expand_limits(x = 0)

g_txi_labs


g_under <- post %>% 
  mutate(
    Region = reorder(Region, p_under)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = p_under, y = Region)) +
  scale_x_continuous("Unreported private cases / All cases initiated treatment, %", labels=scales::percent) + 
  expand_limits(x = 0:1)

g_under


g_priunder <- post %>% 
  mutate(
    Region = reorder(Region, 1 - ppm)
  ) %>% 
  ggplot() +
  stat_halfeye(aes(x = 1 - ppm, y = Region)) +
  #geom_pointinterval(aes(x = M, xmin = L2, xmax = U2, y = State)) + 
  scale_x_continuous("Of people with TB treated by private sector, \nproportion not notified", labels=scales::percent) + 
  expand_limits(x = 0:1)

g_priunder


g_bind_under <- ggpubr::ggarrange(
  g_txi + labs(subtitle = "(A)"), 
  g_priunder + labs(subtitle = "(B)"), 
  common.legend = T, legend = "bottom")

g_bind_under


g_bind_under_abs <- ggpubr::ggarrange(
  g_txi_abs + labs(subtitle = "(A)"), 
  g_priunder + labs(subtitle = "(B)")  +
    scale_y_discrete(position = "right"), 
  common.legend = T, legend = "bottom")

g_bind_under_abs


g_bind_under_labs <- ggpubr::ggarrange(
  g_txi_labs + labs(subtitle = "(A)"), 
  g_priunder + labs(subtitle = "(B)")  +
    scale_y_discrete(position = "right"), 
  common.legend = T, legend = "bottom")

g_bind_under_labs


ggsave(g_ppv_pri, filename = here::here("docs", "figs", "g_rg_ppv.png"), width = 6, height = 5)
ggsave(g_dur_pri, filename = here::here("docs", "figs", "g_rg_dur.png"), width = 6, height = 5)
ggsave(g_drug, filename = here::here("docs", "figs", "g_rg_drug.png"), width = 6, height = 5)
ggsave(g_drugT, filename = here::here("docs", "figs", "g_rg_drugt.png"), width = 6, height = 5)
ggsave(g_txi, filename = here::here("docs", "figs", "g_rg_txi.png"), width = 6, height = 5)
ggsave(g_under, filename = here::here("docs", "figs", "g_rg_under.png"), width = 6, height = 5)
ggsave(g_priunder, filename = here::here("docs", "figs", "g_rg_priunder.png"), width = 6, height = 5)
ggsave(g_bind_under, filename = here::here("docs", "figs", "g_rg_bunder.png"), width = 10, height = 6)
ggsave(g_bind_under_abs, filename = here::here("docs", "figs", "g_rg_baunder.png"), width = 10, height = 6)
ggsave(g_bind_under_labs, filename = here::here("docs", "figs", "g_rg_blaunder.png"), width = 10, height = 6)





