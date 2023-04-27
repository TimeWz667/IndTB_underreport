library(tidyverse)



set.seed(11667)

n_sample <- 300

post <- read_csv(here::here("out", "cs_11", "post.csv"))

n_sample <- min(nrow(post), n_sample)

sel <- post[sample.int(nrow(post), n_sample), ]


tar <- read_csv(here::here("data", "targets_india.csv"))

prev <- local({
  prev <- tar %>% filter(startsWith(Index, "Prev") | Index == "PrCSIPub" | Index == "TBLikeUt")
  prev <- as.list(setNames(prev$M, prev$Index))

})


txo <- tar %>% 
  filter(Index == "TxSucc" | Index == "TxDie") %>% 
  group_by(Index, Tag) %>% 
  summarise(M = weighted.mean(M, N))



prev



js <- list(
  pars = sel,
  prev = prev,
  txo = txo
)

dir.create(here::here("out", "pars"), showWarnings = F)

jsonlite::write_json(js, here::here("out", "pars", "pars_India.json"), digits = 8, auto_unbox = T)


