library("car")
data(Davis)
Davis

X <- cbind(Davis$weight, Davis$height)
plot(X[, 1],
     X[, 2],
     pch = 16,
     xlab = "weight",
     ylab = "height")

# 手順2.2 (p.42参照)
# あらかじめある所与のパーセント値αに基づき、カイ二乗分布から閾値athを求めておく
th <- qchisq(0.99, 1)

# 手順2.2 1) 標本平均と標本共分散行列の計算
mx <- colMeans(X) # 標本平均

# 中心化したデータ行列の計算。`%*%` は行列の積。
# 先に`matrix(1, nrow(X), 1) %*% mx`を計算して、身長・体重の平均値の行列を作ってから、
# 観測値の行列Xから引いている。
Xc <- X - matrix(1, nrow(X), 1) %*% mx # 中心化したデータ行列

# 標本共分散行列の計算
# t()は転置行列を得る関数。`t(Xc) %*% Xc` で(x' - u)同士を掛けて足し合わせている
Sx <- t(Xc) %*% Xc / nrow(X) # 標本共分散行列


# 手順2.2 2) 異常度としてマハラノビス距離を計算
# a <- rowSums((Xc %*% solve(Sx)) * Xc) # 異常度
# 本文では上のコードが記述されているが、正誤表より以下を正しい異常度としている
a <- colSums(t(Xc) * solve(Sx, t(Xc))) # 異常度

plot(a, xlab = "index", ylab = "anomaly score")
lines(0:200, rep(th, length(0:200)), col = "red", lty = 2)
