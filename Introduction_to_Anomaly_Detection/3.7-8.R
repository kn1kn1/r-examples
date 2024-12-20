library(mclust)
library(car)

# 混合正規分布モデルによるクラスタリング
X <- Davis[-12, c("weight", "height")] # データ行列。第12番目の標本は除去
result <- Mclust(X) # クラスタリングの実行。これだけ
print(summary(result, parameters = TRUE)) # 計算結果を画面に出す
plot(result)

# `plot(result)`の箇所を実行すると、
# 以下のように`Selection:`とプロンプトが出てプロットの種類を聞かれるので1〜4で選択する。
# p.92 図3.8は、`2: classification`を出力したもののようだ。
#
# > plot(result)
# Model-based clustering plots:
#
# 1: BIC
# 2: classification
# 3: uncertainty
# 4: density
#
# Selection: 1
# Model-based clustering plots:
# :

# 異常度を計算してプロットする
pi <- result$parameters$pro # 混合比を取り出す
X <- Davis[, c("weight", "height")] # 12番目も含めた全データ行列
XX <-
  cdens(modelName = result$modelName,
        X,
        parameters = result$parameters)
a <- -log(as.matrix(XX) %*% as.matrix(pi)) # 異常度を一気に計算
plot(a, xlab = "index", ylab = "anomaly score")
