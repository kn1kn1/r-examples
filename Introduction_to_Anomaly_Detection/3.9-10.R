# kernlabパッケージに実装されている1クラス支持ベクトル分類器を使って異常検知
library(kernlab)

# サンプルの作成
x <-
  rbind(matrix(rnorm(120), ncol = 2), matrix(rnorm(120, mean = 3), ncol = 2))
x <- scale(x)

# RBFカーネルを使って1クラス支持ベクトル分類器を学習
rbf <- rbfdot(sigma = 0.5) # RBFカーネルのパラメータを設定

# nuは1/(CN)に対応するパラメータで、ここでは0.1という値を与えています。
# これは大雑把にいえば、外れ値として検出する標本の割合に対応しています。
ocsvm <- ksvm(x,
              type = "one-svc",
              kernel = rbf,
              nu = 0.1)

colorcode <- rep(0, nrow(x)) # プロットされる店の色を格納するベクトル
colorcode[ocsvm@alphaindex] <- 1 # 支持ベクトル(外れ値)となる標本の色を「1(黒色)」にセット
# 支持ベクトルについて
#  p.100 `あたかもそれらの標本(外れ値)が球面を「支えて」いるのごとしで、それが
#  「支持ベクトルデータ記述法(support vector data description)」という名前の由来です`

# 色分けされた点をプロット
plot(x, pch = 21, bg = colorcode)
