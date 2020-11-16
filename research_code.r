# 2020/11/06
library(tidyverse)
library(readxl)
# スーツのデータを用いる
suits_16 <- read_excel("researchData/5-3-16.xlsx",sheet = 2)
suits_16
suits_17 <- read_excel("researchData/5-3-17.xlsx",sheet = 2)

suits_18 <- read_excel("researchData/5-3-18.xlsx",sheet = 2)

年度によってサンプル数が違うことがわかる

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
item_suits18_all <- item_suits18 %>%
  drop_na(Question)
# 見てみるけど
ncol(suits_18)
nrow(item_suits18_all)
# 書き出し処理
readr::write_excel_csv(item_suits18_all, "item_suits_all.csv")

# 欠損値の確認
library(skimr)
suits_18 %>%
  skimr::skim()
# 欠損値はないことがわかる