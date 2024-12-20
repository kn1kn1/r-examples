# R に標準で含まれている nottem データに対し、自己回帰モデルによる異常検知を実行

# Nottingham Castle の月ごとの平均気温（華氏）
Dtr <- nottem[1:120] # 訓練データ
xi <- nottem[121:240] # 検証データ
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

y2 <- y / 5 # プロット用に実測値を1/5尺度に
plot(
  y2,
  ylab = "",
  type = "l",
  ylim = c(0, 13),
  main = paste0("nottem: r = ", r)
)
lines(a, lty = 2) # 異常度を点線で重ね合わせる
