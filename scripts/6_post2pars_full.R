library(tidyverse)
library(jsonlite)

set.seed(11667)

n_sample <- 2000

dir.create(here::here("docs", "pars"), showWarnings = F)


## National level
post <- read_csv(here::here("out", "tx_11", "post.csv"))

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


js <- list(
  pars = sel,
  prev = prev,
  txo = txo
)


jsonlite::write_json(js, here::here("docs", "pars", "pars_overall.json"), digits = 8, auto_unbox = T)


## Regional level

regions <- dir("out/tx_11_region")
regions <- regions[startsWith(regions, "post_")]
regions <- gsub("post_", "", regions)
regions <- gsub(".csv", "", regions)


for (reg in regions) {
  reg <- glue::as_glue(reg)
  
  post <- read_csv(here::here("out", "tx_11_region", "post_" + reg + ".csv"))
  
  n_sample <- min(nrow(post), n_sample)
  sel <- post[sample.int(nrow(post), n_sample), ]
  
  tar <- read_csv(here::here("data", "targets_Region_" + reg + ".csv"))
  
  prev <- local({
    prev <- tar %>% filter(startsWith(Index, "Prev") | Index == "PrCSIPub" | Index == "TBLikeUt")
    prev <- as.list(setNames(prev$M, prev$Index))
  })
  
  
  txo <- tar %>% 
    filter(Index == "TxSucc" | Index == "TxDie") %>% 
    group_by(Index, Tag) %>% 
    summarise(M = weighted.mean(M, N))
  
  
  js <- list(
    pars = sel,
    prev = prev,
    txo = txo
  )
  
  
  jsonlite::write_json(js, here::here("docs", "pars", "pars_r_" + reg + ".json"), digits = 8, auto_unbox = T)
  
}


## State level

states <- dir("out/tx_11_state")
states <- states[startsWith(states, "post_")]
states <- gsub("post_", "", states)
states <- gsub(".csv", "", states)


for (st in states) {
  st <- glue::as_glue(st)
  
  post <- read_csv(here::here("out", "tx_11_state", "post_" + st + ".csv"))
  
  n_sample <- min(nrow(post), n_sample)
  sel <- post[sample.int(nrow(post), n_sample), ]
  
  tar <- read_csv(here::here("data", "targets_State_" + st + ".csv"))
  
  prev <- local({
    prev <- tar %>% filter(startsWith(Index, "Prev") | Index == "PrCSIPub" | Index == "TBLikeUt")
    prev <- as.list(setNames(prev$M, prev$Index))
  })
  
  
  txo <- tar %>% 
    filter(Index == "TxSucc" | Index == "TxDie") %>% 
    group_by(Index, Tag) %>% 
    summarise(M = weighted.mean(M, N))
  
  
  js <- list(
    pars = sel,
    prev = prev,
    txo = txo
  )
  
  
  jsonlite::write_json(js, here::here("docs", "pars", "pars_s_" + st + ".json"), digits = 8, auto_unbox = T)
  
}



