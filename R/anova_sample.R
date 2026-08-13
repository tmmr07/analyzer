# データの読み込み
data <- read.csv("/Users/k22129kk/Desktop/tmp/java/singapore.csv", header = TRUE, stringsAsFactors = TRUE)

# データの先頭6行を表示
head(data)

# データをデータフレームで表示
str(data)

# Q-Qプロット
qqnorm(data$score)

# Q-Qプロットに直線を描画
qqline(data$score, col = "red")

# シャピロ・ウィルク検定
shapiro.test(data$score)

# シャピロ・ウィルク検定を各データ群に対して実行
by(data$score, data$Algorithm, shapiro.test)

# バートレット検定を実行
bartlett.test(score ~ Algorithm, data = data)

# 分散分析の実行
anova_result <- aov(score ~ Algorithm, data = data)

# 分散分析の結果を表示
summary(anova_result)

# t検定（等分散を仮定）
t.test(score ~ Algorithm, data = data, var.equal = TRUE)

# テューキー検定
tukey_result <- TukeyHSD(anova_result)

# テューキー検定の結果を表示
print(tukey_result)




