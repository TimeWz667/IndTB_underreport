library(tidyverse)
library(tidybayes)

theme_set(theme_bw() + theme(text = element_text(family = "sans")))

load(here::here("docs", "tabs", "post_incre.rdata"))


g_p_under <- post %>% 
  filter(Scenario %in% c("tx_11", "dx_11", "cs_11")) %>% 
  ggplot() +
  stat_halfeye(aes(x = p_under, y = Scenario)) +
  scale_x_continuous("Proportion cases not notified, %", labels = scales::percent) +
  scale_y_discrete("Modelling from ", labels = c(tx_11="Treatment", dx_11="Diagnosis", cs_11="Care-seeking")) + 
  expand_limits(x=0)
  
g_p_under


ggsave(g_p_under, filename=here::here("docs", "figs", "g_sens_incre_under.png"), width=4, height=5)


