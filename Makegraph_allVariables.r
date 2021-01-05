# データの読み込みから特定の変数系を一気にグラフにする
library(tidyverse)
library(ggplot2)
library(readxl)

# データと設問項目の読み込み
suits_18 <- read_excel("researchData/5-3-18.xlsx",sheet = 2)

item_suits18 <- read_excel("researchData/5-3-18.xlsx",sheet = 1)

# 1. 具体的にQ2の項目を使用する
item_Q2_number <- item_suits18_all %>%
  filter(grepl("Q2_", Question)) %>%
  select(Question)

item_Q2 <- item_suits18_all %>%
  filter(grepl("Q2_", Question)) %>%
  select(Title)

# 2. Q2のみの表を作成しておく
Q2_newlabel <- paste(item_Q2_number$Question, item_Q2$Title)
Q2 <- suits_18 %>%
  select(Q2_1:Q2_38)　%>%
  setNames(Q2_newlabel)

# 3. Q2系は38項目
for (i in 1:38){
  if (i == 1){
    x <- c(item_Q2_number[i,1])
  }
  else {
    x <- append(x, item_Q2_number[i,1])
  }
}

for (i in 1:38){
  if (i == 1){
    y <- c(item_Q2[i,1])
  }
  else {
    y <- append(y, item_Q2[i,1])
  }
}

# 名称のベクトルを作成する
for (i in 1:38){
  if (i == 1){
    Q2_label_vector <- c(paste(x[1],y[1]))
  }
  else {
    Q2_label_vector <- append(Q2_label_vector, paste(x[i],y[i]))
  }
}

# Q2のみの評価から名称を引っこ抜く
table_col_vector <- c(colnames(Q2))

tmp_for_paste <- paste("`",Q2_label_vector, sep = "")
Q2_label_vector <- paste(tmp_for_paste,"`", sep = "")

## 保存場所と保存名を明確にするためにそのためのリストを作成する
tmp_for_save_place <- paste("MasterResearch_R/output/",Q2_newlabel, sep = "")
save_place <- paste(tmp_for_save_place,".png", sep = "")
save_place <- gsub(" ","",save_place)

# 実際にグラフを生成して，画像を出力する
for (i in 1:38){
  p <- ggplot() +
    geom_bar(data = Q2, mapping = aes_string(x = Q2_label_vector[i])) +
    scale_x_continuous(breaks = seq(0,10,1)) +
    theme_bw()
  ggsave(file = save_place[i],plot = p,
         dpi = 100, width = 6.4, height = 4.8)
}

