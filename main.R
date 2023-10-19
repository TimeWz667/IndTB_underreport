
## Model fitting

dir("scripts")

source(here::here("scripts", "1_fit_tx.R"))
source(here::here("scripts", "1_fit_tx_subnational.R"))
source(here::here("scripts", "1_fit_tx_ts.R"))
source(here::here("scripts", "1_fit_dx.R"))
source(here::here("scripts", "1_fit_cs.R"))
source(here::here("scripts", "4_sens_ppm_shape.R"))
source(here::here("scripts", "4_sens_ppm_lo.R"))


source(here::here("scripts", "2_collect_post.R"))
source(here::here("scripts", "5_collect_sens.R"))


## Visualisation

source(here::here("scripts", "3_vis_posterior.R"))
source(here::here("scripts", "3_vis_state.R"))
source(here::here("scripts", "3_vis_region.R"))
source(here::here("scripts", "5_vis_incre.R"))
source(here::here("scripts", "5_vis_ppm_lo.R"))
source(here::here("scripts", "5_vis_ppm_shape.R"))



## Export as parameters

source(here::here("scripts", "6_post2pars.R"))
