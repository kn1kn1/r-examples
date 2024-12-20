library(MASS)
cc <-
  c(
    "Min.Price",
    "Price",
    "Max.Price",
    "MPG.city",
    "MPG.highway",
    "EngineSize",
    "Horsepower",
    "RPM",
    "Rev.per.mile",
    "Fuel.tank.capacity",
    "Length",
    "Wheelbase",
    "Width",
    "Turn.circle",
    "Weight"
  )
mask <- is.element(colnames(Cars93), cc) # 上記15変数のみを選ぶマスク
Xc <- t(scale(Cars93[, mask])) # 中心化したデータ行列の作成
colnames(Xc) <- t(Cars93[, "Make"]) # 車種(make)を変数名にする

# 手順 5.1 1) a) 正常部分空間の計算
# 次元数M(=15)が標本数N(=93)より小さい場合、
# 散布行列Sについての固有値方程式(5.8)を解く
S <- Xc %*% t(Xc) # 散布行列の作成
evd <- eigen(S) # 固有値分解
plot(evd$values,
     type = "b",
     xlab = "index",
     ylab = "eigenvalue")

# 手順 5.1 2) a) 異常度の計算
# 式(5.28)を用いて異常度を計算する。
m <- 2
x2 <- t(evd$vectors[, 1:m]) %*% Xc # 正常部分空間内の成分を計算
a1 <- colSums(Xc * Xc) - colSums(x2 * x2) # 異常度を全訓練標本に対し計算
idx <- order(a1, decreasing = T)[1:6] # 異常度上位6つ
print(a1[idx])


# 手順 5.1 1) b) 正常部分空間の計算
# 標本数N(=93)のほうが次元数M(=15)より小さい場合、
# (↑今回は当てはまっていない)
# グラム行列についての固有値方程式(5.12)を解く
G <- t(Xc) %*% Xc # グラム行列の作成
evd2 <- eigen(G) # 固有値分解

# 手順 5.1 2) b) 異常度の計算
# 式(5.30)を用いて異常度を計算する。
# (5.29)のUmの計算
Lam_12 <- diag(evd2$values[1:m] ^ {
  -1 / 2
}) # 固有値の対角行列(diag)の -1/2 乗
# (5.31)のz'の計算
xx2 <- Lam_12 %*% t(evd2$vectors[, 1:m]) %*% t(Xc) %*% Xc # 正常成分
aa1 <- colSums(Xc * Xc) - colSums(xx2 * xx2) # 異常度
idx <- order(aa1, decreasing = T)[1:3] # 異常度上位3つ
print(aa1[idx])
