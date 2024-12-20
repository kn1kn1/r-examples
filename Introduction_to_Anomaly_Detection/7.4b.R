# Keogh らによる心電図データに対し、自己回帰モデルによる異常検知を実行

# Keogh らによる心電図データ
# http://www.cs.ucr.edu/~eamonn/discords/qtdbsel102.txt
# http://www.cs.ucr.edu/~eamonn/discords/
dt <- read.table(file = "data/qtdbsel102.txt")
xi <- dt[3001:6000, 2]
# 検証データ部分を図示
plot(xi, ylab = "ECG", type = "l")

Dtr <- dt[1:3000, 2] # 訓練データ
Tt <- length(xi)
ar.model <- ar(Dtr) # 自己回帰モデルの学習
print(ar.model) # 学習されたモデルの表示

r <- ar.model$order # AIC で選択した次数
alpha <- ar.model$ar # AIC で選択した係数
xmean <- ar.model$x.mean
sig2 <- ar.model$var.pred
N <- Tt - r
X <- t(embed(xi - xmean, r))[, 1:N] # スライド窓による回帰データの準備
ypred <- t(X) %*% alpha + xmean # 予測値の計算
y <- xi[(1 + r):Tt] # 実測値
a <- (y - as.numeric(ypred)) ^ 2 / sig2 # 異常度の計算

y2 <- 10 * y + 60 # プロット用に適当に尺度変換
plot(
  y2,
  ylab = "",
  type = "l",
  ylim = c(0, 120),
  main = paste0("ECG: r = ", r)
)
lines(a, lty = 2) # 異常度を点線で重ね合わせる
