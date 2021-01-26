# 異なるデータで実行してみる

library(bnlearn)
library(tidyverse)
library(Rgraphviz)

data("coronary")
head(coronary)
tb.coronary <- as_tibble(coronary)
head(tb.coronary)
tb.coronary %>%
  skimr::skim()

# 仮にダミー変数化したいのなら
library(dummies)
dummied_dat <- dummies::dummy.data.frame(dat, sep = "_", names = "Sepal.Length")
# みたいにやる

# ダミー変数化ではなく0, 1で操作する
tb.coro_01 <- tb.coronary %>%
  mutate(Smoking = if_else(Smoking == "yes", 1, 0)) %>%
  mutate(`M. Work` = if_else(`M. Work` == "yes", 1, 0)) %>%
  mutate(`P. Work` = if_else(`P. Work` == "yes", 1, 0)) %>%
  mutate(Pressure = if_else(Pressure == ">14", 1, 0)) %>%
  mutate(Proteins = if_else(Proteins == ">3", 1, 0)) %>%
  mutate(Family = if_else(Family == "pos", 1, 0)) 

# ホワイトリストとブラックリストの作成
# あまり知識がないので放置

# ネットワーク学習
# ブーストラップ
str.coro = boot.strength(tb.coro_01, R = 200, algorithm = "hc",s
                         algorithm.args = list(score = "bic-g"))

avg.coro = averaged.network(str.coro)
strength.plot(avg.coro, str.coro, shape = "ellipse")

plot(str.coro)


# 閾値を可視化
threshold_value <- attr(str.coro, "threshold")
plot(str.coro)
abline(v = threshold_value, col = "red", lty = 2, lwd = 2)
abline(v = 0.05, col = "blue", lty = 2, lwd = 2)

# 0.7で試してみる
# 閾値を変更することでネットワークを変える
avg.coro2 = averaged.network(str.coro, threshold = 0.7)
strength.plot(avg.coro2, str.coro, shape = "ellipse")

# 作ったネットワークの比較ができる
compare(cpdag(avg.coro, wlbl = FALSE), cpdag(avg.coro2, wlbl = FALSE))

par(mfrow = c(1,2))
graphviz.compare(avg.coro, avg.coro2,
                 shape = "ellipse", main = c("first","second"))

# パラメータ学習
fitted_coro2 = bn.fit(avg.coro2, tb.coro_01)
fitted_coro2

# モデルの評価
# 個々の円弧の関係を見る
# まず何かの変数のみで評価する
xval = bn.cv(tb.coro_01,
             bn = "hc",
             algorithm.args = list(blacklist = NULL, whitelist = NULL),
             loss = "cor-lw",
             loss.args = list(target = "Family", n = 200),
             runs = 10)

err = numeric(10)

for (i in 1:10) {
  tt = table(unlist(sapply(xval[[i]], '[[', "observed")),
             unlist(sapply(xval[[i]], '[[', "predicted")) > 0.50)
  err[i] = (sum(tt) - sum(diag(tt))) / sum(tt)
}

summary(err)

# すべての変数でおこなう
coro_colnames <- c(colnames(tb.coro_01))
coro_colnames[2] <- "`M. Work`"
coro_colnames[3] <- "`P. Work`"
coro_colnames

predcor = structure(numeric(6),
                    names = coro_colnames)

for (var in names(predcor)){
  xval = bn.cv(tb.coro_01,
               bn = "hc",
               algorithm.args = list(blacklist = NULL, whitelist = NULL),
               loss = "cor-lw",
               loss.args = list(target = var, n = 200),
               runs = 10)
  predcor[var] = mean(sapply(xval, function(x) attr(x, "mean")))
} # エラー発生
# もしかしたら浮いてる変数はダメかもしれない


# Pressureを落としてネットワークを構成する
## coro_2
tb.coro_01_2 <- tb.coro_01 %>%
  select(-(Pressure))

str.coro_2 = boot.strength(tb.coro_01_2, R = 200, algorithm = "hc",
                         algorithm.args = list(score = "bic-g"))
avg.coro_2 = averaged.network(str.coro_2)
strength.plot(avg.coro_2, str.coro_2, shape = "ellipse")

threshold_value <- attr(str.coro_2, "threshold")
plot(str.coro_2)
abline(v = (threshold_value + 0.1), col = "red", lty = 2, lwd = 2)
abline(v = (threshold_value - 0.1), col = "blue", lty = 2, lwd = 2)

# 閾値変更
avg.coro_2_2 = averaged.network(str.coro_2, threshold = 0.25)
strength.plot(avg.coro_2_2, str.coro_2, shape = "ellipse")

# fitting
fitted_coro2 = bn.fit(avg.coro_2_2, tb.coro_01_2)
fitted_coro2

# cvの前準備
xval = bn.cv(tb.coro_01_2,
             bn = "hc",
             algorithm.args = list(blacklist = NULL, whitelist = NULL),
             loss = "cor-lw",
             loss.args = list(target = "Proteins", n = 200),
             runs = 10)

err = numeric(10)

for (i in 1:10) {
  tt = table(unlist(sapply(xval[[i]], '[[', "observed")),
             unlist(sapply(xval[[i]], '[[', "predicted")) > 0.50)
  err[i] = (sum(tt) - sum(diag(tt))) / sum(tt)
}

summary(err)
# クロスバリデーション
coro_colnames2 <- c(colnames(tb.coro_01_2))
coro_colnames2
coro_colnames2[2] <- "`M. Work`"
coro_colnames2[3] <- "`P. Work`"

predcor = structure(numeric(5),
                    names = coro_colnames2)

for (var in names(predcor)){
  xval = bn.cv(tb.coro_01_2,
               bn = "hc",
               algorithm.args = list(blacklist = NULL, whitelist = NULL),
               loss = "cor-lw",
               loss.args = list(target = var, n = 200),
               runs = 10)
  predcor[var] = mean(sapply(xval, function(x) attr(x, "mean")))
}
