score <-
  c(0.19, 0.86, 0.17, 0.12, 0.04, 0.78, 0.16, 0.51, 0.57, 0.27) # 異常度
anomaly <- c(F, T, F, F, F, T, F, T, F, F) # 正解データ(異常かどうかのフラグ)
data0 <- cbind(score, anomaly)
rownames(data0) <-
  c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
barplot(data0[, "score"], ylim = c(0, 1), col = data0[, "anomaly"]) # インデックス順で表示

data1 <- data0[order(score, decreasing = T),]
score_sorted <- data1[, "score"]
anomaly_sorted <- data1[, "anomaly"]
barplot(data1[, "score"], ylim = c(0, 1), col = data1[, "anomaly"]) # 異常度で降順に表示

n_total <- length(anomaly) # 全標本数
n_anom <- sum(anomaly) # 異常標本数
n_norm <- n_total - n_anom # 正常標本数
coverage <-
  rep(0, n_total) # 異常標本精度、異常標本網羅率(coverage, recall)を格納する変数
detection <- rep(1, n_total) # 正常標本精度、正答率(detection ratio)を格納する変数
for (i in c(1:n_total)) {
  n_detectedAnom <- sum(anomaly_sorted[1:i])
  n_detectedNorm <- (n_total - i) - sum(anomaly_sorted[-(1:i)])
  coverage[i] <- n_detectedAnom / n_anom
  detection[i] <- n_detectedNorm / n_norm
}

# 異常標本精度、異常標本網羅率(coverage, recall)を破線(dot)でプロット
plot(
  score_sorted,
  coverage,
  type = "b",
  ylim = c(0:1),
  xlab = "threthold",
  ylab = "coverage(dot) and detection ratio(solid)",
  lty = 2
)
# 正常標本精度、正答率(detection ratio)を実線(solid)でプロット
lines(score_sorted, detection, type = "b")
