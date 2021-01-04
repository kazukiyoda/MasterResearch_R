# ベイジアンネットワーク
library(bnlearn)
library(tidyverse)

# シンプルなデータに対して簡単な操作

data("learning.test")
df <- learning.test
tibble::glimpse(df)

# ベイジアンネットワークの構造学習
# グラフの形そのものを学習するタスクで，データから確率変数間の矢印を学習する．

# 条件付き独立性を用いる
# GSアルゴリズム
res <- gs(df)
plot(res, main = "the Grow-Shrink(GS)")

# IABMアルゴリズム
res2 <- iamb(df)
plot(res2, main = "the Incremental Association")

# 最後に構造学習にスコア関数を用いるHill-Climbingアルゴリズムを試す
res3 <- hc(df)
plot(res3, main = "the hill-climbing(HC)")

# bn.fit()に得られた構造を与えることでパラメータ学習を行う．
# methodにmleを指定すると最尤法，bayesを指定するとベイズ法により学習を行う．
fitted <- bn.fit(res3, data = df, method = "bayes")

# cpdist()とcpquery()により結果から条件付き確率を取得できる．
particles <- cpdist(fitted, nodes = "A", evidence = (B == "C"))
prop.table(table(particles))


# 本命
## irisデータセットでやってみよう
iris
str(iris)

# データ探索
# 分布の確認

par(mfrow = c(2,3), mar = c(4,2,2,2))
for(var in c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width")){
  x = iris[, var]
  hist(x, prob = TRUE, xlab = var, ylab = "", main = "",col = "ivory")
  lines(density(x), lwd = 2, col = "red")
  curve(dnorm(x, mean = mean(x), sd = sd(x)), form = min(x), to = max(x),
        add = TRUE, lwd = 2, col = "blue")
}

# ネットワーク学習
## 1. どんなネットワークを持つのか
## 2. ネットワーク構造をもとにパラメータがどうなっているのか
library(Rgraphviz)

# 山登り法：各変数が正規分布に従っているほか，変数間が線形で説明できるとき
wl = matrix(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
            ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from","to")))
wl
iris_bayes <- iris %>%
  select(Sepal.Length:Petal.Width)
dag = hc(iris_bayes, score = "bic-g", whitelist = wl)
dag
graphviz.plot(dag, shape = "ellipse", highlight = list(arcs = wl))

# そうではないので，ブートストラップ法で行う
str.iris = boot.strength(iris_bayes, R = 200, algorithm = "hc",
                         algorithm.args = list(score = "bic-g",
                                               whitelist = wl))
head(str.iris)

# 機械側が判断した閾値を表示してくれる
plot(str.iris)

threshold_value <- attr(str.iris, "threshold")
# plotで閾値以上の強さを持つネットワークを取り出す
avg.iris = averaged.network(str.iris)
strength.plot(avg.iris, str.iris, shape = "ellipse", highlight = list(arcs = wl))

plot(str.iris)
abline(v = threshold_value, col = "red", lty = 2, lwd = 2)
abline(v = 0.855, col = "blue", lty = 2, lwd = 2)

# ここで密度を確認して，高い密度の値を選択してそこを閾値にする
avg.simpler = averaged.network(str.iris, threshold = 0.85)
strength.plot(avg.simpler, str.iris, shape = "ellipse", highlight = list(arcs = wl))

# パラメータ学習に行く前に
fitted.simple = bn.fit(avg.iris, iris_bayes)
# the graph is only partially directed.というエラーが出る
# つまり向行グラフが混ざっているからダメなので，ネットワークを再考すべきである．
head(str.iris)
# とりあえずブラックリスト入れるか
bl = tiers2blacklist(matrix(c("Sepal.Length", "Petal.Length"), ncol = 2,
                            byrow = TRUE,dimnames = list(NULL,c("from",to))))
bl
str.iris02 = boot.strength(iris_bayes, R = 200, algorithm = "hc",
                         algorithm.args = list(score = "bic-g",
                                               whitelist = wl,
                                               blacklist = bl))
plot(str.iris02)
avg.iris02 = averaged.network(str.iris02)
strength.plot(avg.iris02, str.iris02, shape = "ellipse", highlight = list(arcs = wl))

# うまくいかないので，とりあえずデータセットを変えたいがHPに入れない・・・

# あまりよろしくないが，DAGのほうを利用させてもらう
fitted.dag = bn.fit(dag, iris_bayes)
fitted.dag
fitted.dag$Sepal.Length
# gRainでより厳密な推論が可能だが変数が多いと止まるとのこと
library(gRain)

# 交差検証はbn.cvで可能
xval = bn.cv(iris_bayes, bn = "hc", algorithm.args = list(whitelist = wl),
             loss = "cor-lw", loss.args = list(target = "Petal.Width", n = 200), runs = 10)
xval
err = numeric(10)
for(i in 1:10) {
  tt = table(unlist(sapply))
}

# 別のデータセットでやってみる