# MASS パッケージにある UScrime を使って、都市の犯罪率 y をp.171 表6.1にある14の変数から予測する
library(MASS)
X <- UScrime[,-c(2, 16)] # 第2, 16変数を除いた14個の変数
M <- ncol(X)
y <- UScrime[, 16] # 第16番目がyの犯罪率に対応
N <- length(y)

lambdas <- seq(0, 5, length = 50) # リッジ回帰のλの候補
# lm.ridge()にリッジ回帰のλを最適に選ぶ機能が実装されている
model <- lm.ridge(y ~ ., cbind(X, y), lambda = lambdas)
bestIdx <- which.min(model$GCV) # 一般化交差確認法の評価値が最小のもの
coefs <- coef(model)[bestIdx, ] # 回帰係数
lam <- model$lambda[bestIdx] # 選択されたλの値
ypred <-
  as.matrix(X) %*% as.matrix(coefs[2:15]) + coefs[1] # リッジ回帰により算出された予測値

# 実際の値と予測値を比較する（p.172の図6.3(a)）
li <- c(0, 2000) # 図示範囲
plot(
  y,
  ypred,
  xlab = "y actual",
  ylab = "y predict",
  xlim = li,
  ylim = li
)

# それぞれの州の異常度を計算・図示する（p.172の図6.3(b)）
sig2 <-
  (lam * sum(coefs[2:15] ^ 2) + sum(as.numeric(ypred) - y) ^ 2) /
  N
X_ <- t(scale(X, scale = F)) # 中心化したデータ行列
H <- t(X_) %*% solve(X_ %*% t(X_) + lam * diag(M), X_) # H行列
TrHN <- sum(diag(H)) / N # HのトレースをNで割ったもの
a <- (as.numeric(ypred) - y) ^ 2 / ((1 - TrHN) ^ 2 * sig2) # 異常度
plot(a, xlab = "index", ylab = "anomaly score")
th <- sort(a)[N * (1 - 0.05)] # 閾値
lines(0:50, rep(th, length(0:50)), col = "red", lty = 2)
