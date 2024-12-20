library(kernlab)
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

m <- 2 # 主成分の数
sig <- 0.1 # カーネルパラメータ
li <- c(-6, 7) # 図示範囲

# kpca 関数を使うことで手順5.4(p.151)のステップが1行で書けている
kpc <-
  kpca(
    t(Xc),
    kernel = "rbfdot",
    kpar = list(sigma = sig),
    features = m
  )
Zt <- rotated(kpc) # 主成分空間における座標

# 座標の図示
plot(
  Zt[, 1],
  Zt[, 2],
  xlab = "1st PC",
  ylab = "2nd PC",
  cex = 3,
  col = 3,
  xlim = li,
  ylim = li,
  main = paste0("sig: ", sig)
)

# 車種番号をドット内に書き込む
text(Zt[, 1],
     Zt[, 2],
     c(1:93),
     cex = 0.8,
     xlim = li,
     ylim = li)
