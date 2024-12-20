library("car")
data(Davis)
Davis
hist(Davis$weight, xlim = c(35, 105), breaks = 14)

mu <- mean(Davis$weight)
s2 <- mean((Davis$weight - mu) ^ 2)
c(mu, s2)

a <- (Davis$weight - mu) ^ 2 / s2
# カイ二乗分布（自由度1, スケール因子1）から1%水準の閾値の算出
# `自由度1, スケール因子1` であるのは、p.20の定理2.1から
th <- qchisq(0.99, 1)
plot(a, xlab = "index", ylab = "anomaly score")
lines(0:200, rep(th, length(0:200)), col = "red", lty = 2)
