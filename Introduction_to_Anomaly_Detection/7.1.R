# FNN パッケージを使った最近傍法による異常部位発見
library(FNN)

# Keogh らによる心電図データ
# http://www.cs.ucr.edu/~eamonn/discords/qtdbsel102.txt
# http://www.cs.ucr.edu/~eamonn/discords/
X <- read.table(file = "data/qtdbsel102.txt")
# 検証データ部分を図示
plot(X[3001:6000, 2], ylab = "ECG", type = "l")

w <- 100 # 窓幅
nk <- 1 # 近傍数1。最も近いものとの距離のみを計算する。
# p.197 「関数 score に関しては、k = 1と選んだ上で、最近傍までの距離の値
# そのものを異常度とすることが多いと思います」
# p.198 「近傍数k > 1の場合には、近傍距離の平均を考える、などの拡張も可能」

# 訓練データ
Xtr <- X[1:3000, 2]
# embed() でスライド窓によるベクトル化
Dtr <- embed(Xtr, w)

# 検証データ
Xt <- X[3001:6000, 2]
Dt <- embed(Xt, w)

# 最近傍までの距離を計算
# knnx.dist() で、p.197 手順7.1の二つの関数 dist と score を実装している模様
d <- knnx.dist(Dtr, Dt, k = nk)
a <- d[, 1]

# 異常度の図示
plot(a, ylab = "anomaly score", type = "l")
