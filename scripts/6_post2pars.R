library(tidyverse)



set.seed(11667)

n_sample <- 300

post <- read_csv(here::here("out", "cs_11", "post.csv"))

n_sample <- min(nrow(post), n_sample)

sel <- post[sample.int(nrow(post), n_sample), ]


prev <- local({
  tar <- read_csv(here::here("data", "targets_india.csv"))
  prev <- tar %>% filter(startsWith(Index, "Prev") | Index == "PrCSIPub" | Index == "TBLikeUt")
  prev <- as.list(setNames(prev$M, prev$Index))
  
})




dim(post)


