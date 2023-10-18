library(tidyverse)


theme_set(theme_bw())

d = read_csv(here::here("out", "cs_independent", "post.csv"))


g_ppv <- d %>% 
  ggplot() +
  geom_point(aes(x = ppv_pub, y = ppv_eng), alpha = 0.2) +
  geom_abline(slope = 1) +
  scale_x_continuous("PPV, Public", labels = scales::percent) +
  scale_y_continuous("PPV, Engaged private", labels = scales::percent) +
  expand_limits(x = 0, y = 0)


ggsave(g_ppv, filename = here::here("out", "cs_independent", "g_ppv.png"), width = 5, height = 5)



g_cdx <- d %>% 
  ggplot() +
  geom_point(aes(x = sens_cdx, y = spec_cdx), alpha = 0.2) +
  geom_abline(slope = 1) +
  scale_x_continuous("Sensitivity, CDx", labels = scales::percent) +
  scale_y_continuous("Specificity, CDx", labels = scales::percent)

ggsave(g_cdx, filename = here::here("out", "cs_independent", "g_cdx.png"), width = 5, height = 5)



d %>% 
  ggplot() +
  geom_density(aes(x = ppv_pri), alpha = 0.2)
