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
tmp_for_save_place <- paste("MasterResearch_R/output/Q2",Q2_newlabel, sep = "")
save_place <- paste(tmp_for_save_place,".png", sep = "")
save_place <- gsub(" ","",save_place)

## for文とラベルのベクトルを利用することで可能とした
for (i in 1:38){
  p <- ggplot() +
    geom_bar(data = Q2, mapping = aes_string(x = Q2_label_vector[i])) +
    scale_x_continuous(breaks = seq(0,10,1)) +
    theme_bw()
  ggsave(file = save_place[i],plot = p,
         dpi = 100, width = 6.4, height = 4.8)
}

# 自己表現に関する変数Q26

Q26 <- suits_18 %>%
  select(`Q26[1]`:`Q26[17]`)
summary(Q26)

item_suits18 %>%
  filter(str_detect(Question,"Q26"))

ggplot() +
  geom_bar(data = Q26, mapping = aes(x = `Q26[1]`)) +
  theme_bw()

Q26_sum <- c(apply(Q26,2,sum))

#なんか作業したけど忘れた

tb.Q26_sum2 <- tb.Q26_sum %>%
  select(`Q26[1]`:`Q26[17]`)

# ラベル名を更新した
tb.Q26_sum <- tb.Q26_sum2 %>%
  setNames(Q26_label_vector)

# グラフ生成
p <- ggplot() +
  geom_bar(data = tb.Q26_sum, mapping = aes_string(x = Q26_label_vector[i])) +
  theme_bw()

ggsave(file = save_place[i],plot = p,
       dpi = 100, width = 6.4, height = 4.8)

library(ggplot2)
library(ggsci)

# このdfをsumで完成させるべきだったので、再考する
df <- data.frame(
  sample = c("A1", "A2", "B1", "B2", "C1", "C2"),
  weight = c(0.32, 0.33, 0.21, 0.22, 0.37, 0.36)
)

g <- ggplot(df, aes(x = sample, y = weight, fill = sample))
g <- g + geom_bar(stat = "identity")
g <- g + scale_fill_nejm()
plot(g)

# 各アンケート項目の変数を見ることで，どんな回答法が見る
suits_18 %>%
  select(`SQ4[1]`:`SQ4[37]`) %>%
  rowSums()

suits_18 %>%
  select(`SQ5[1]`:`SQ5[16]`) %>%
  summary()

suits_18 %>%
  select(Q9_1:Q9_2) %>%
  skimr::skim()

suits_18 %>%
  select(Q17_1:Q17_2) %>%
  skimr::skim()
