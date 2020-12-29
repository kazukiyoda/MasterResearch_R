# 列名を一次元配列から作成する
# 二度手間になるけど、考え付いた方法

item_Q2_number <- item_suits18_all %>%
  filter(grepl("Q2_", Question)) %>%
  select(Question)

item_Q2_number

item_Q2 <- item_suits18_all %>%
  filter(grepl("Q2_", Question)) %>%
  select(Title)

item_Q2
x

# tmpに番号を入れていく

for (i in 1:38){
  if (i == 1){
    x <- c(item_Q2_number[i,1])
  }
  else {
    x <- append(x, item_Q2_number[i,1])
  }
}

# 同様に

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

# 同じものなのか判定する
for (i in 1:length(Q2_label_vector)) {
  if(Q2_newlabel[i] == Q2_label_vector[i]) {
    print("True")
  } 
  else {
    print("False")
  }
}

## 結果としてはすべて同じ？っぽいので、従来法と変わりがないらしい。
## つまりエラーはggplot側にあるのかも
## 次に考えられるものとしてはテーブルの列名側に問題があるかも

# 作成したテーブルQ2の列名を抽出する
table_col_vector <- c(colnames(Q2))

# 同じように見てみる
for (i in 1:length(Q2_label_vector)) {
  if(table_col_vector[i] == Q2_label_vector[i]) {
    print("True")
  } 
  else {
    print("False")
  }
}

# やっぱり結果はみんな同じだったは、GG

# 謎エラーがstack overflowにあったので拝借
str <- Q2_label_vector[1]
parse(text = str)

str <- "`Q2_1 品質の良さ（素材や耐久性など）`"
parse(text = str)

# テーブルのラベル側に問題があるようなのです。
# 実際問題、めんどうなので、Q2_label_vectorに　``　これをつけることにする
# sep = ""を忘れないこと

tmp_for_paste <- paste("`",Q2_label_vector, sep = "")
Q2_label_vector <- paste(tmp_for_paste,"`", sep = "")
Q2_label_vector[1]
