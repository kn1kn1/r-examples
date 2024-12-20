library(car)
library(KernSmooth)

# 手順3.4（カーネル密度推定におけるバンド幅選択）p.80参照
#  Hの候補値をあらかじめH1,H2,…と用意する
#  Hの候補それぞれについて積分二乗誤差(3.35)の値を計算し、記録する
#  最小の積分二乗誤差を与えるHiを最適解として採用する
x <- Davis[, c("weight", "height")]
h <- c(dpik(x$weight), dpik(x$height)) # カーネル幅の自動推定
est <-
  bkde2D(x, bandwidth = h, gridsize = c(10 ^ 3, 10 ^ 3)) # 格子点上でpを計算
d <- list(x = est$x1,
          y = est$x2,
          z = est$fhat)
image(d,
      col = terrain.colors(7),
      xlim = c(35, 110),
      ylim = c(145, 200))
contour(d, add = T)

# 以降は標本数nとカーネル行列Kを予め用意する必要があるが、分からず…
# https://tjo.hatenablog.com/entry/2017/05/23/190000
# にも、
# 「ただし、p.83の異常度の評価のところのカーネル行列Kがどこで出てきたのか
# （もしくはどう入れるべきか）なのかが分からず。。。一旦ここではスキップ
# してあります。やり方が分かったら追記します。」
# とあり、かつ追記が無かったので私もスキップすることにした。