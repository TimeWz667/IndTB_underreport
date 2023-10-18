library(tidyverse)


pr_pub <- read_csv(here::here("data", "PrPublic.csv"))


pr_pub <- pr_pub %>% 
  mutate(
    Pr = N_PriOnPub / N_Private,
    Var = Pr * (1 - Pr) / N_Private,
    k = ifelse(N_Private != 100, N_Txi_Pri / N_Private, NA)
  ) 


pr_pub_region <- pr_pub %>% 
  filter(!is.na(N_Private)) %>%
  group_by(Region) %>% 
  summarise(
    Pr1 = weighted.mean(Pr, N_Txi_Pri),
    Var1 = weighted.mean(Var, N_Txi_Pri)
  ) %>% 
  left_join(
    pr_pub %>% 
      group_by(Region) %>% 
      summarise(N_Txi_Pri = sum(N_Txi_Pri))
  )

pr_pub_agg <- bind_rows(
  pr_pub_region,
  pr_pub_region %>% 
    summarise(
      Pr2 = weighted.mean(Pr1, N_Txi_Pri),
      Var2 = weighted.mean(Var1, N_Txi_Pri),
      N_Txi_Pri = sum(N_Txi_Pri)
    ) %>% 
    mutate(
      Pr1 = Pr2,
      Var1 = Var2,
      Region = "India"
    )
) %>% 
  mutate(
    Pr2 = mean(Pr2, na.rm = T)
  ) %>% 
  select(- N_Txi_Pri)


pr_public_reformed <- pr_pub %>% 
  left_join(pr_pub_agg) %>% 
  mutate(
    Source = ifelse(is.na(Pr), "Region", "Exact"),
    Var = ifelse(is.na(Pr), Var1, Var),
    Pr = ifelse(is.na(Pr), Pr1, Pr),
    Source = ifelse(is.na(Pr), "Nation", Source),
    Var = ifelse(is.na(Pr), Var2, Var),
    Pr = ifelse(is.na(Pr), Pr2, Pr),
    Source = ifelse(Region == "India", "Nation", Source)
  ) %>% 
  select(- Pr1, - Pr2, - Var1, - Var2) %>% 
  mutate(
    N_Private = N_Txi_Pri,
    N_PriOnPub = round(N_Private * Pr),
    abm = Pr * (1 - Pr) / Var - 1,
    al = Pr * abm,
    be = abm - al,
    Pr_L = qbeta(0.025, al, be),
    Pr_U = qbeta(0.975, al, be)
  ) %>%
  select(- abm, - k)


write_csv(pr_public_reformed, here::here("data", "PrPublicPrior.csv"))

