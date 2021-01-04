# 異なるデータで実行してみる

library(bnlearn)
library(tidyverse)

data("coronary")
head(coronary)
tb.coronary <- as_tibble(coronary)
head(tb.coronary)
tb.coronary %>%
  skimr::skim()
