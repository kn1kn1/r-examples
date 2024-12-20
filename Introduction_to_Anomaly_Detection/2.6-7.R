# MASSパッケージに含まれるroadデータを使ったマハラノビス=タグチ方の手順
library(MASS)

X <- road / road$drivers # drivers(運転者数)により各列を割る
X <- as.matrix(log(X[, -2] + 1)) # 対数変換

mx <- colMeans(X) # 標本平均

# 中心化したデータ行列の計算。`%*%` は行列の積。
# 先に`matrix(1, nrow(X), 1) %*% mx`を計算して、身長・体重の平均値の行列を作ってから、
# 観測値の行列Xから引いている。
Xc <- X - matrix(1, nrow(X), 1) %*% mx # 中心化したデータ行列

# 標本共分散行列の計算
# t()は転置行列を得る関数。`t(Xc) %*% Xc` で(x' - u)同士を掛けて足し合わせている
Sx <- t(Xc) %*% Xc / nrow(X) # 標本共分散行列

# 異常度として「1変数あたりのマハラノビス距離を」計算
# p.43で出てきた異常度について `ncol(X)` で割ることで1変数あたりの異常度としている
# a <- rowSums((Xc %*% solve(Sx)) * Xc) / ncol(X) # 1変数あたりの異常度
# 本文では上のコードが記述されているが、正誤表より以下を正しい異常度としている
a <- colSums(t(Xc) * solve(Sx, t(Xc))) / ncol(X) # 1変数あたりの異常度

plot(a,
     xlab = "index",
     ylab = "anomaly score",
     ylim = c(-1, 30) / ncol(X))
lines(0:30, rep(1, length(0:30)), col = "red", lty = 2) # 閾値1の高さに線を引く

# 閾値を超えた5つの州は、左からAlaska, Calif, DC, Maine, Mont
# そのうち、CalifについてSN比解析を行う
xc_prime <- Xc["Calif",] # 中心化行列から Calif のデータを取り出す

# SN比の計算
# `xc_prime ^ 2`が, p.51(2.47)式の(x' - u)^2に相当し、
# `diag(Sx)`がσ2に相当し、diag関数によりSxの対角行列を取っている
SN1 <- 10 * log10(xc_prime ^ 2 / diag(Sx)) # 全変数一気に SN 比を計算
barplot(SN1) # 棒グラフのプロット
