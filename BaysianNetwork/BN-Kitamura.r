# 北村ベイジアンネットワーク

# ベイジアンネットワーク
library(tidyverse)
library(bnlearn)
library(gRain)
library(readxl)
library(skimr)

# directryの設定
setwd("~/修論研究")

suits_18 <- read_excel("researchData/5-3-18.xlsx",sheet = 2)
item_suits18 <- read_excel("researchData/5-3-18.xlsx",sheet = 1)

df_suits18_kitamura_model <- suits_18 %>%
  select(Q1,Q2_1:Q2_38,Q7_1:Q7_13,Q16_1:Q16_10,`Q26[1]`:`Q26[17]`)

md_Q1 <- median(df_suits18_kitamura_model$Q1)
md_Q2 <- median(df_suits18_kitamura_model$Q1)

df_suits18_kitamura_model %>%
  select(Q2_1:Q2_38) %>%
  skimr::skim() %>%
  filter(stat == "median")

# 萎えたのでpythonでやる
write_csv(df_suits18_kitamura_model, "df_preKitmura_suits.csv")

# また読み込む

# ネットワーク学習
str.suits18.kita = boot.strength(df_suits18_kitamura_model, R = 10,
                                 algorithm = "hc",
                                 algorithm.args = list(score = "bic-g"),
                                 debug = TRUE)
head(str.suits18.kita)
plot(str.suits18.kita)

avg.suits18.kita = averaged.network(str.suits18.kita)

strength.plot(avg.suits18.kita, str.suits18.kita, shape = "ellipse")

fitted.suits18.kita = bn.fit(avg.suits18.kita, df_suits18_kitamura_model)
# DAGになってないのでもちろんのことエラー

# ブラックリストとホワイトリストの作成
