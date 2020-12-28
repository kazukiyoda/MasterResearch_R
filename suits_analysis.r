library(tidyverse)
library(skimr)
library(ggplot2)
library(ggsci)
library(gridExtra)
library(stringr)

suits_18 %>%
  select(Q1) %>%
  summary()

g <- ggplot() +
  geom_histogram(data = suits_18, mapping = aes(x = Q1))
  
# 軸を制御するためscaleを利用する
ggplot() +
  geom_bar(data = suits_18, mapping = aes(x = Q1)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,1000,100)) +
  scale_x_continuous(breaks = seq(0,10,1))

ggplot() +
  geom_boxplot(data = suits_18, mapping = aes(y = Q1)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,10,1))

# Q2は満足度の小項目

## アンケート項目から抜き出し
## Question 項目にQ2_と含まれている行を抜き出して
## そのタイトルをリストで保存する
item_Q2 <- item_suits18_all %>%
  filter(grepl("Q2_", Question)) %>%
  select(Title)

item_Q2_number <- item_suits18_all %>%
  filter(grepl("Q2_", Question)) %>%
  select(Question)

## アンケート番号と具体的な質問を結合
Q2_newlabel <- paste(item_Q2_number$Question, item_Q2$Title)
Q2_newlabel

## 完成
Q2 <- suits_18 %>%
  select(Q2_1:Q2_38)　%>%
  setNames(Q2_newlabel)

ggplot() +
  geom_bar(data = Q2, mapping = aes(x = `Q2_1 品質の良さ（素材や耐久性など）`)) +
  theme_bw()


