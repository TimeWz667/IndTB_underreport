library(tidyverse)
library(tidybayes)

theme_set(theme_bw() + theme(text = element_text(family = "sans")))

vis_all = F

if (vis_all) {
  for (sc in c("cs_11", "dx_11", "tx_11", "tx_01", "tx_10", "tx_00")) {
    
    post <- read_csv(here::here("out", sc, "post.csv"))
    
    g_ppm <- post %>% 
      ggplot() + 
      stat_halfeye(aes(x = ppm), alpha = 0.7) + 
      scale_x_continuous("Engaged private visits / all private visits, %", labels = scales::percent_format()) +
      scale_y_continuous("") + 
      expand_limits(x = c(0, 1)) +
      theme(axis.text.y = element_blank())
    
    g_ppm
    
    
    g_dur_pri <- post %>% 
      ggplot() + 
      stat_halfeye(aes(x = dur_pri), alpha = 0.7) + 
      scale_x_continuous("Treatment duration, on private drug, month", labels = scales::number_format(scale = 12)) +
      scale_y_continuous("") + 
      expand_limits(x = 0) +
      theme(axis.text.y = element_blank())
    
    
    g_dur_pri
    
    g_ppv_pri <- post %>% 
      ggplot() + 
      stat_halfeye(aes(x = ppv_pri), alpha = 0.7) + 
      scale_x_continuous("Positive Predictive Value, in the private sector, %", labels = scales::percent_format()) +
      scale_y_continuous("") + 
      expand_limits(x = c(0, 1)) +
      theme(axis.text.y = element_blank())
    
    
    g_bind <- ggpubr::ggarrange(g_ppm, g_ppv_pri, g_dur_pri, ncol=1)
    
    g_bind
    
    ggsave(g_bind, filename = here::here("out", sc, "g_posterior.png"), width = 5, height = 5)
    
  }
  
  
}



load(here::here("docs", "tabs", "post_main.rdata"))


post %>% 
  ggplot() +
  geom_point(aes(x = ppv_pri, y = dur_pri, colour = Scenario), alpha = 0.2) +
  facet_wrap(.~Scenario)


post01 <- post %>% 
  filter(
    Scenario == "Drug sales data alone" | Scenario == "Drug sales + prevalence survey data"
  ) %>% 
  mutate(
    Scenario = ifelse(Scenario == "Drug sales data alone", "Drug sales data only", "Drug sales with TBPS"),
    Scenario = factor(Scenario, levels = c("Prior distribution", "Drug sales data only", "Drug sales with TBPS"))
  )



#### Plotting ----

g_cross <- post01 %>% 
  filter(Scenario != "Prior distribution") %>% 
  ggplot() +
  geom_rect(xmin=1 / 24, xmax=1, ymin=0.2, ymax=0.75, alpha = 0.01, 
            linetype = 2, fill = NA,
            aes(colour = "Prior distribution")) +
  geom_point(aes(x = dur_pri, y = ppv_pri, colour = Scenario), alpha = 0.4, position = "identity") +
  scale_x_continuous("Average duration of first-line treatment, private sector (months)", labels = scales::number_format(scale = 12))  +
  scale_y_continuous("Positive predictive value, private sector diagnosis", labels = scales::percent) +
  scale_color_discrete("") +
  expand_limits(x = 0)

g_cross


g_ppv <- post01 %>% 
  ggplot() +
  # geom_rect(xmin=0.2, xmax=0.85, ymin=0, ymax= 0.5 / (0.85 - 0.2), alpha = 0.01, 
  #           colour = "black", linetype = 2, fill = NA) +
  geom_vline(xintercept = c(0.2, 0.75), linetype = 2) + 
  stat_halfeye(aes(x = ppv_pri, fill = Scenario), alpha = 0.4, position = "identity") +
  scale_x_continuous("Positive predictive value, private sector diagnosis", labels = scales::percent) +
  scale_y_continuous("Probability density") +
  scale_fill_discrete("") +
  guides(fill = guide_legend(reverse = F)) + 
  theme(axis.text.y = element_blank())
  
g_ppv


g_dur <- post01 %>% 
  ggplot() +
  # geom_rect(xmin=1 / 24, xmax=2, ymin=0, ymax= 0.5 / (2 - 1 / 24), alpha = 0.01, 
  #           colour = "black", linetype = 2, fill = NA) +
  geom_vline(xintercept = c(1 / 24, 1), linetype = 2) + 
  stat_halfeye(aes(x = dur_pri, fill = Scenario), alpha = 0.4, position = "identity") +
  scale_x_continuous("Average duration of first-line treatment, private sector (months)", labels = scales::number_format(scale = 12)) +
  scale_y_continuous("Probability density") +
  scale_fill_discrete("") +
  guides(fill = guide_legend(reverse = F)) + 
  theme(axis.text.y = element_blank())


g_dur



g_post <- ggpubr::ggarrange(g_ppv + labs(subtitle = "(A)"), 
                  g_dur + labs(subtitle = "(B)"), 
                  common.legend = T, legend = "bottom")


g_post



g_drugtime <- post01 %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_drug_time, fill = Scenario), alpha = 0.4, position = "identity") +
  scale_x_continuous("million person-year", labels=scales::number_format(scale = 1e-6), limits = c(0, 1.5e6)) +
  scale_y_continuous("Probability density") +
  scale_fill_discrete("") +
  guides(fill = guide_legend(reverse = F)) + 
  theme(axis.text.y = element_blank())

g_drugtime


g_unreported <- post01 %>% 
  ggplot() +
  stat_halfeye(aes(x = tp_pri_txi, fill = Scenario), alpha = 0.4, position = "identity") +
  scale_x_continuous("Unreported number with TB initiating private treatment, \nmillion", labels=scales::number_format(scale = 1e-6), limits = c(0, 3e6)) +
  scale_y_continuous("Probability density") +
  scale_fill_discrete("") +
  guides(fill = guide_legend(reverse = F)) + 
  theme(axis.text.y = element_blank())

g_unreported


g_priunder <- post01 %>% 
  ggplot() +
  stat_halfeye(aes(x = ppm, fill = Scenario), alpha = 0.4, position = "identity") +
  #geom_pointinterval(aes(x = M, xmin = L2, xmax = U2, y = State)) + 
  scale_x_continuous("Of people with TB treated by private sector, \nproportion non-notified", labels=scales::percent) + 
  scale_y_continuous("Probability density") +
  expand_limits(x = 0:1) + 
  theme(axis.text.y = element_blank())

g_priunder


g_bind_under <- ggpubr::ggarrange(
  g_unreported + labs(subtitle = "(A)"), 
  g_priunder + labs(subtitle = "(B)"), 
  common.legend = T, legend = "bottom")

g_bind_under


g_attload <- ggpubr::ggarrange(
  g_drugtime + labs(subtitle = "(A) Caseloads with drugs from private providers"), 
  g_unreported + labs(subtitle = "(B) Number of non-notified people with TB initiating treatment in the private sector "), 
  common.legend = T, legend = "bottom")

g_attload




ggsave(g_ppv, filename = here::here("docs", "figs", "g_post_ppv.png"), width = 6, height = 4)
ggsave(g_dur, filename = here::here("docs", "figs", "g_post_dur.png"), width = 6, height = 4)
ggsave(g_post, filename = here::here("docs", "figs", "g_post.png"), width = 9, height = 6)
ggsave(g_cross, filename = here::here("docs", "figs", "g_post_cross.png"), width = 9, height = 6)
ggsave(g_attload, filename = here::here("docs", "figs", "g_post_case.png"), width = 9, height = 6)
ggsave(g_bind_under, filename = here::here("docs", "figs", "g_post_under.png"), width = 9, height = 6)


