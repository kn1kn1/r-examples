lambda <- 0.3 # Box-Cox パラメータの設定
par(ask = T) # 複数の図を順繰りに表示(`次の図を見るためには <Return> キーを押して下さい:`のプロンプト表示)する設定
x <- sunspot.month # データ(太陽黒点数の月次変動)の読み込み
xx <- ((x + 1) ^ lambda - 1) / lambda # Box-Cox 変換の実行
plot(x, type = "l") # 元の時系列データの表示
plot(xx, type = "l") # 変換後の時系列データの表示
plot(density(x), main = "", xlab = "") # 値の分布の表示（元データ）
plot(density(xx), main = "", xlab = "") # 値の分布の表示（変換後のデータ）
