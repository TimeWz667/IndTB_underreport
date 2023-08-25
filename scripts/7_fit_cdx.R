library(tidyverse)
library(rstan)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)


## Data loading

targets <- read_csv(here::here("data", "targets_india.csv"))

targets %>% 
  filter(Year == 2019) %>% data.frame()


drug <- targets %>% filter(Index == "DrugTime") %>% 
  filter(Year == 2019)

tx <- targets %>% filter(Index == "PrTxiPub") %>% 
  mutate(X = round(M * N))

det <- targets %>% 
  filter(Year == 2019) %>% 
  filter(Index == "TxI") %>% 
  mutate(
    N_Txi = round(N * M),
    N_Det = N
  )


ds <- local({
  N_Test_SSM = 13914911
  N_Test_Xpert = 4120552 
  N_Det_Pub = 1688427
  N_Det_Eng = 733694
  N_DetBac = 513050 + 548981
  N_DetCDx = 835930 + 1037210
  N_Txi_Pub = 1527464
  N_Txi_Eng = 554980
  N_Pop = 1425775850
  
  PropXpert_Eng = (215594 + 262160) / (3483130 + 2365739)
  # BacPerXpert_Eng = 68556 / (215594 + 262160)
  
  N_Test_Xpert_Eng = round(N_Test_Xpert * PropXpert_Eng)
  # N_DetBac_Eng = round(N_Test_Xpert_Eng * BacPerXpert_Eng)
  
  list(
    N_Det_Pub = N_Det_Pub,
    N_Det_Eng = N_Det_Eng,
    N_DetBac = N_DetBac,
    # N_DetBac_Pub = N_DetBac - N_DetBac_Eng,
    # N_DetBac_Eng = N_DetBac_Eng,
    N_Test_SSM_Pub = N_Test_SSM,
    N_Test_NAAT_Pub = N_Test_Xpert - N_Test_Xpert_Eng,
    N_Test_NAAT_Eng = N_Test_Xpert_Eng,
    N_DetCDx = N_Det_Pub + N_Det_Eng - N_DetBac,
    # N_DetCDx_Pub = N_Det_Pub - (N_DetBac - N_DetBac_Eng),
    # N_DetCDx_Eng = N_Det_Eng - N_DetBac_Eng,
    N_Txi_Pub = N_Txi_Pub,
    N_Txi_Eng = N_Txi_Eng,

    Pop = N_Pop,
    
    tp_bac = c(0.73984, 0.544, 0.7225, 0),
    fp_bac = c(0.0336, 0.017, 0.017, 0),
    test_tb_ssm = c(0.85, 0.85, 0, 0),
    test_tb_naat = c(0.306, 0, 0.85, 0),
    test_nontb_ssm = c(0.85, 0.85, 0, 0),
    test_nontb_naat = c(0.833, 0, 0.85, 0),
    
    Tx = tx$N,
    Tx_Pub = tx$X,
    Drug = drug$M,
    Drug_Std = drug$Error,
    
    p_csi_pub = 0.483,
    dur_upper = 2
  )
})


targets <- tribble(
  ~Index, ~value,
  "DrugTime", ds$Drug,
  "DetPub", ds$N_Det_Pub / ds$Pop,
  "DetEng", ds$N_Det_Eng / ds$Pop,
  "TestSSM", ds$N_Test_SSM_Pub / ds$Pop,
  "TestNAAT", (ds$N_Test_NAAT_Pub + ds$N_Test_NAAT_Eng) / ds$Pop,
  "NotiBac", ds$N_DetBac / ds$Pop,
  "NotiCDx", ds$N_DetCDx / ds$Pop,
  "TxiPub", ds$N_Txi_Pub / ds$Pop,
  "TxiEng", ds$N_Txi_Eng / ds$Pop
)



tar <- read_csv(here::here("data", "targets_india.csv"))

prev <- local({
  prev <- tar %>% filter(startsWith(Index, "Prev") | Index == "PrCSIPub" | Index == "TBLikeUt")
  prev <- as.list(setNames(prev$M, prev$Index))
})


txo <- tar %>% 
  filter(Index == "TxSucc" | Index == "TxDie") %>% 
  group_by(Index, Tag) %>% 
  summarise(M = weighted.mean(M, N))



for(model_key in c("cs_exclusive", "cs_free", "cs_independent")) {
  
  model <- rstan::stan_model(here::here("stan", glue::as_glue(model_key) + ".stan"))
  
  post <- rstan::sampling(model, data=ds, iter=1e4, warmup=1e4 - 1000)
  
  stan_dens(post, pars=c("p_tb", "r_test"))
  stan_dens(post, pars=c("alg_pub"))
  pairs(post, pars=c("alg_pub"))
  stan_dens(post, pars=c("alg_eng"))
  
  stan_dens(post, pars=c("ppv_pub", "ppv_eng", "ppv_pri"))
  
  stan_scat(post, pars=c("sens_cdx", "spec_cdx"))
  
  
  stan_scat(post, pars=c("dur_pri", "p_txi_pri"))
  
  stan_dens(post, pars=c("p_csi_ppm"))
  
  
  ext <- extract(post) %>% 
    data.frame() %>% 
    as.tibble() %>% 
    mutate(
      p_csi_pub = ds$p_csi_pub
    )
  
  tab <- as.data.frame(summary(post)$summary)
  tab$Name <- rownames(tab)
  tab <- tab %>% as_tibble() %>% relocate(Name)
  
  
  dir.create(here::here("out", model_key), showWarnings = F)
  save(post, file = here::here("out", model_key, "post.rdata"))
  write_csv(tab, file = here::here("out", model_key, "summary.csv"))
  write_csv(ext, file = here::here("out", model_key, "post.csv"))
  
  
  to_fit <- ext %>% 
    mutate(
      R_DetPub = r_det_bac_pub + r_det_cdx_pub,
      R_DetEng = r_det_bac_eng + r_det_cdx_eng,
      R_DrugTime = drug_time,
      R_TestSSM = r_test_ssm_pub,
      R_TestNAAT = r_test_naat_pub + r_test_naat_eng,
      R_NotiBac = r_det_bac_pub + r_det_bac_eng,
      R_NotiCDx = r_det_cdx_pub + r_det_cdx_eng,
      R_TxiPub = (r_det_bac_pub + r_det_cdx_pub) * p_txi_pub,
      R_TxiEng = (r_det_bac_eng + r_det_cdx_eng) * p_txi_eng
    ) %>% 
    select(starts_with("R", ignore.case = F)) %>% 
    pivot_longer(everything(), names_to = "Index") %>% 
    mutate(
      Index = gsub("R_", "", Index)
    )
  
  
  
  g_gof <- to_fit %>% 
    ggplot() +
    geom_density(aes(x = value)) +
    geom_vline(data = targets, aes(xintercept = value)) + 
    facet_wrap(.~Index, scales = "free_x") +
    scale_x_continuous("rate, per 100k", labels = scales::number_format(scale = 1e5))
  
  g_gof
  
  ggsave(g_gof, filename = here::here("out", model_key, "g_gof.png"), width = 7, height = 6)
  
  
  sel <- ext[sample.int(nrow(ext), 2000), ]
  
  js <- list(
    pars = sel,
    prev = prev,
    txo = txo
  )
  
  jsonlite::write_json(js, here::here("docs", "pars", "pars_" + glue::as_glue(model_key) + ".json"), digits = 8, auto_unbox = T)
  
}











