library(tidyverse)
library(readxl)
library(skimr)
library(ggplot2)
library(ggsci)
library(gridExtra)
# スーツのデータを用いる
suits_16 <- read_excel("researchData/5-3-16.xlsx",sheet = 2)
suits_16
suits_17 <- read_excel("researchData/5-3-17.xlsx",sheet = 2)

suits_18 <- read_excel("researchData/5-3-18.xlsx",sheet = 2)

# 年度によってサンプル数が違うことがわかる

# SQ1:性別 1がmen
# SQ2:年齢
# SQ7:スーツを購入した店(ブランド)
# Q1:総合満足度
# Q2:各要素における満足度(品質や着心地の良さなど)
hist(suits_16$SQ1,breaks = seq(1,2,0.5)
     ,main = "紳士服専門店：性別 2016年",xlab = "性別(1:男性,2:女性)",ylab = "頻度")
hist(suits_16$SQ2,breaks = seq(0,90,1)
     ,main = "紳士服専門店：年齢 2016年",xlab = "年齢",ylab = "頻度")
hist(suits_16$SQ3,breaks = seq(0,50,1)
     ,main = "紳士服専門店：都道府県 2016年",xlab = "年齢",ylab = "人数")

# 外れ値の検出を行うために箱ひげ図の作成を行う
boxplot(suits_16$SQ1, xlab = "性別 2016年")
# 年齢
boxplot(suits_16$SQ2, xlab = "年齢 2016年")
boxplot(suits_17$SQ2, xlab = "年齢 2017年")
boxplot(suits_18$SQ2, xlab = "年齢 2018年")
suits_16

library(lattice)
pairs(suits_16)

map(suits_16,boxplot)

summary(suits_16$SQ1)
summary(suits_16$SQ2)
# サンプル数
nrow(suits_16)

# 2018年の結果を見ていく
#変数と質問項目を一致させるために整理する
item_suits18 <- read_excel("researchData/5-3-18.xlsx",sheet = 1)
# 空白を削除する
## Quesion列で空白が存在してる行を消していく
item_suits18_all <- item_suits18 %>%
  drop_na(Question)
# 見てみるけど
ncol(suits_18)
nrow(item_suits18_all)
# 書き出し処理
readr::write_excel_csv(item_suits18_all, "item_suits18_all.csv")

## 2017年と2016年に対しも同様の処理を行うことで質問リストを比較する
### 2017年
item_suits17 <- read_excel("researchData/5-3-17.xlsx",sheet = 1)
item_suits17_all <- item_suits17 %>%
  drop_na(Question)
item_suits16 <- read_excel("researchData/5-3-16.xlsx",sheet = 1)
item_suits16_all <- item_suits16 %>%
  drop_na(Question)
readr::write_excel_csv(item_suits17, "item_suits17.csv")
readr::write_excel_csv(item_suits17_all, "item_suits17_all.csv")
readr::write_excel_csv(item_suits16, "item_suits16.csv")
readr::write_excel_csv(item_suits16_all, "item_suits16_all.csv")

# summaryの上位互換でいろんな要素を同時に出力できる
suits_18 %>%
  skimr::skim()

# スクリーニング調査に対してデータの確認を行った．
suits18Screening_result_list <- suits_18 %>%
  select(SQ1:SQ12) %>%
  skimr::skim() 

# それ以降の設問も見てみる
suits18Survey_result_list <- suits_18 %>%
  select(Q1:Q36) %>%
  skimr::skim()

suits_18 %>%
  select(`SQ9[3]`,SQ10) %>%
  filter(is.na(SQ10))

# 2018年紳士服
# ヒストグラム
# 男女のヒストグラム
g <- ggplot(data = suits_18, aes(x = SQ1)) +
  geom_histogram(binwidth = 1) +
  scale_fill_nejm()
plot(g)

# 年齢
g_old <- ggplot(data = suits_18, aes(x = SQ2)) +
  geom_histogram()
plot(g_old)

# 都道府県
g_prefecture <- ggplot(data = suits_18, aes(x = SQ3)) +
  geom_histogram()
plot(g_prefecture)

# 都道府県ごとの人数を知りたいので，きっちりわける必要がある

# 2016年紳士服
g_gender_16 <- ggplot(data = suits_16, aes(x = SQ1)) +
  geom_histogram(binwidth = 1)
plot(g_gender_16)
# 2017年紳士服
g_gender_17 <- ggplot(data = suits_17, aes(x = SQ1)) +
  geom_histogram(binwidth = 1)
plot(g_gender_17)

#genderを並べて画像に出力
suits_gender_compare <- gridExtra::grid.arrange(g_gender_16,g_gender_17,g)
ggsave(file = "紳士服_男女比較.png",plot = suits_gender_compare,
       dpi = 100, width = 6.4, height = 4.8)
       
## スクリーニングだけでアンケート内容を見てみる
library(stringr)
#item_suits18_screening <- item_suits18 %>%
#  filter(str_detect(Question,"SQ"))
#item_suits17_screening <- item_suits17 %>%
#  filter(str_detect(Question,"SQ"))
#item_suits16_screening <- item_suits16 %>%
#  filter(str_detect(Question,"SQ"))
## 上の場合選択肢が消されてしまう

item_suits18_screening <- item_suits18 %>%
  select(Question,CtgNo,Title) %>%
  head(224)

item_suits17_screening <- item_suits17 %>%
  select(Question,CtgNo,Title) %>%
  head(224)

item_suits16_screening <- item_suits16 %>%
  select(Question,CtgNo,Title) %>%
  head(220)
## 書き出し
readr::write_excel_csv(item_suits18_screening, "item_suits18_screening.csv")
readr::write_excel_csv(item_suits17_screening, "item_suits17_screening.csv")
readr::write_excel_csv(item_suits16_screening, "item_suits16_screening.csv")

# 本項目の内容を見ていく

#item_suits18_survey
item_suit18_row <- nrow(item_suits18)
item_suit18_row #データフレームの行数

# 本調査が始まる行番号の抽出
item_suits18_survey_begin <- match("Q1",item_suits18$Question) #QuestionがQ1の行を特定

# 選択肢を全部含む場合
item_suits18_survey <- item_suits18 %>%
  select(Question,CtgNo,Title) %>%
  slice(item_suits18_survey_begin:item_suit18_row)

readr::write_excel_csv(item_suits18_survey, "item_suits18_survey.csv")

# item_suirs17_survey
item_suit17_row <- nrow(item_suits17)
item_suit17_row #データフレームの行数

# 本調査が始まる行番号の抽出
item_suits17_survey_begin <- match("Q1",item_suits17$Question) #QuestionがQ1の行を特定

item_suits17_survey <- item_suits17 %>%
  select(Question,CtgNo,Title) %>%
  slice(item_suits17_survey_begin:item_suit17_row)

readr::write_excel_csv(item_suits17_survey, "item_suits17_survey.csv")

# item_suirs16_survey
item_suit16_row <- nrow(item_suits16)
item_suit16_row #データフレームの行数

# 本調査が始まる行番号の抽出
item_suits16_survey_begin <- match("Q1",item_suits16$Question) #QuestionがQ1の行を特定

item_suits16_survey <- item_suits16 %>%
  select(Question,CtgNo,Title) %>%
  slice(item_suits16_survey_begin:item_suit16_row)

readr::write_excel_csv(item_suits16_survey, "item_suits16_survey.csv")

# 回答選択肢(QuestionがNAとなる選択肢)を全部消してみる
item_suits18_survey_simple <- item_suits18 %>%
  select(Question,CtgNo,Title) %>%
  slice(item_suits18_survey_begin:item_suit18_row) %>%
  drop_na(Question)

readr::write_excel_csv(item_suits18_survey_simple, "item_suits18_survey_simple.csv")
