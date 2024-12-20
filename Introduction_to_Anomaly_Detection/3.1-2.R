library("car")
library(MASS)

data(Davis)
Davis

N <- length(Davis$weight) # 標本数
mu <- mean(Davis$weight) # 標本平均

# 標準偏差の計算
# sdが`Like var this uses denominator n - 1.`(標本標準偏差を計算している)なので、
# 標準偏差にするためにn - 1を掛けてnで再度割っている
# https://ja.wikipedia.org/wiki/%E6%A8%99%E6%BA%96%E5%81%8F%E5%B7%AE
# si <- sd(Davis$weight) * (N - 1) / N # 標準偏差
# – 誤: si <- sd(Davis$weight)*(N-1)/N #標準偏差
# – 正: si <- sd(Davis$weight)*sqrt((N-1)/N) #標準偏差
si <- sd(Davis$weight)*sqrt((N-1)/N) #標準偏差

kmo <- (mu / si) ^ 2 # モーメント法によるkの推定値
smo <- si ^ 2 / mu # モーメント法によるsの推定値

ml <- fitdistr(Davis$weight, "gamma")
kml <- ml$estimate["shape"] # 最尤法によるkの推定値
sml <- 1 / ml$estimate["rate"] # 最尤法によるsの推定値

a <- Davis$weight / smo - (kmo - 1) * log(Davis$weight / smo)
th <- order(a, decreasing = T)[0.01 * N]
plot(a, ylab = "anomaly score")
lines(0:200, rep(a[th], length(0:200)), col = "red", lty = 2)
