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

mode(item_Q2_number)

## アンケート番号と具体的な質問を結合
Q2_newlabel <- paste(item_Q2_number$Question, item_Q2$Title)
Q2_newlabel

Q2

suits_18

## 完成
Q2 <- suits_18 %>%
  select(Q2_1:Q2_38)　%>%
  setNames(Q2_newlabel)
Q2

## 単体の出力ならこれでオッケー
ggplot() +
  geom_bar(data = Q2, mapping = aes(x = `Q2_1 品質の良さ（素材や耐久性など）`)) +
  theme_bw()

ggplot() +
  geom_bar(data = Q2, mapping = aes(x = ))

d <- colnames(Q2)
mode(d)
mode(Q2_newlabel)



# 変数ごとのグラフを大量に作成する

## 保存場所と保存名を明確にするためにそのためのリストを作成する
tmp_for_save_place <- paste("MasterResearch_R/output/",Q2_newlabel, sep = "")
save_place <- paste(tmp_for_save_place,".png", sep = "")
save_place <- gsub(" ","",save_place)

## for文とラベルのベクトルを利用することで可能とした
for (i in 1:38){
  p <- ggplot() +
    geom_bar(data = Q2, mapping = aes_string(x = Q2_label_vector[i])) +
    theme_bw()
  ggsave(file = save_place[i],plot = p,
         dpi = 100, width = 6.4, height = 4.8)
}
