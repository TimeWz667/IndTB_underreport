library(tidyverse)


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


