# 2混合正規分布を学習して背景雑音を取り除く

# 2つの正規分布の混ざった1000個の標本を作成する
pi0 <- 0.6 # 信号成分の正規分布の出身である確率
pi1 <- 0.4 # 雑音成分の正規分布の出身である確率
mu0 <- 3 # 信号成分の正規分布の平均値
mu1 <- 0 # 雑音成分の正規分布の平均値
sig0 <- 1.0 # 信号成分の正規分布の標準偏差
sig1 <- 3 # 雑音成分の正規分布の標準偏差
N <- 1000
attr <- sample(0:1, N, replace = T, prob = c(pi0, pi1))
x <- rep(-99, N)
x[which(attr == 0)] <- rnorm(length(which(attr == 0)), mu0, sig0)
x[which(attr == 1)] <- rnorm(length(which(attr == 1)), mu1, sig1)

# 作成した標本
x

# 手順 3.2 (1次元混合正規分布の期待値-最大化法) p.70
# 手順 3.2 1) パラメータの初期値を適当に設定する
pi0 <- 0.5
pi1 <- 0.5
mu0 <- 5.0
mu1 <- -5.0
sig0 <- 1.0
sig1 <- 5.0

# 期待値計算と最大化計算を1000回繰り返す
for (iteration in 1:1000) {
  # 手順 3.2 2) 帰属度qnを求める
  piN0 <- pi0 * dnorm(x, mu0, sig0)
  piN1 <- pi1 * dnorm(x, mu1, sig1)
  qn0 <- piN0 / (piN0 + piN1)
  qn1 <- piN1 / (piN0 + piN1)
  pi0 <- sum(qn0) / N
  pi1 <- sum(qn1) / N
  # 手順 3.2 3)
  mu0 <- sum(qn0 * x) / (N * pi0)
  mu1 <- sum(qn1 * x) / (N * pi1)
  sig0 <- sqrt(sum(qn0 * (x - mu0) * (x - mu0)) / (N * pi0))
  sig1 <- sqrt(sum(qn1 * (x - mu1) * (x - mu1)) / (N * pi1))
}

# パラメータを確認する
pi0
pi1
mu0
mu1
sig0
sig1
