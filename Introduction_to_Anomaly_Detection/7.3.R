# 特異スペクトル変換法を用いて変化度を計算

# 人為的にsinを合成して作成した検証データ
set.seed(1)
tt <- 0.1
x1 <- seq(0, 10, by = tt)
x2 <- seq(10.1, 20, by = tt)
x3 <- seq(20.2, 30, by = tt)
y1 <- sin(pi * x1) + rnorm(length(x1), sd = 0.07)
y2 <- sin(2 * pi * x2) + rnorm(length(x2), sd = 0.07)
y3 <- sin(pi * x3) + rnorm(length(x3), sd = 0.07)
xi <- c(y1, y2, y3)
# 検証データ部分を図示
plot(xi, type = "l")

# 以降はパラメータが異なるだけで、実行例 7.2と同じ
w <- 10 # 窓幅w
m <- 2 # パターン数m
k <- 10 # 履歴行列の列サイズk
L <- 5 # ラグL
Tt <- length(xi)
score <- rep(0, Tt) # 変化度の入れ物

for (t in (w + k):(Tt - L  + 1)) {
  tstart <- t - w - k + 1 # 左の行列の範囲
  tend <- t - 1
  X1 <- t(embed(xi[tstart:tend], w)) # 部分時系列を並べた行列を作成
  X1 <- X1[w:1, ] # 時間の順序を逆にする
  
  tstart <- t - w - k + 1 + L # 右の行列の範囲
  tend <- t - 1 + L
  X2 <- t(embed(xi[tstart:tend], w))
  X2 <- X2[w:1, ]
  
  U1 <- svd(X1)$u[, 1:m] # X1 の特異値分解
  U2 <- svd(X2)$u[, 1:m] # X2 の特異値分解
  sig1 <- svd(t(U1) %*% U2)$d[1] # 部分空間同士の重なり度合い
  score[t] <- 1 - sig1 ^ 2 # 変化度の計算
}

# 異常度の図示
plot(score, ylab = "anomaly score", type = "l")
