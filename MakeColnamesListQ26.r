# Q26系
item_Q26_number <- item_suits18_all %>%
  filter(grepl("Q26", Question)) %>%
  select(Question)
item_Q26_number[1,1] <- NA
item_Q26_number <- na.omit(item_Q26_number)

item_Q26 <- item_suits18_all %>%
  filter(grepl("Q26", Question)) %>%
  select(Title)
item_Q26[1,1] <- NA
item_Q26 <- na.omit(item_Q26)

Q26_col <- ncol(Q26)
Q26_col

## ここくそ長いけど汎用性高いから関数にする
for (i in 1:Q26_col){
  if (i == 1){
    x <- c(item_Q26_number[i,1])
  }
  else {
    x <- append(x, item_Q26_number[i,1])
  }
}

for (i in 1:Q26_col){
  if (i == 1){
    y <- c(item_Q26[i,1])
  }
  else {
    y <- append(y, item_Q26[i,1])
  }
}

# 名称のベクトルを作成する
for (i in 1:Q26_col){
  if (i == 1){
    Q26_label_vector <- c(paste(x[1],y[1]))
  }
  else {
    Q26_label_vector <- append(Q26_label_vector, paste(x[i],y[i]))
  }
}
Q26_label_vector

## ここまで関数にする