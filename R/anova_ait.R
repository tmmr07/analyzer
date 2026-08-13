# 必要パッケージの読み込み
install.packages("car")     # Levene検定のために一度だけ必要
install.packages("ggplot2") # 必要であれば
library(car)

# データの読み込み
data <- read.csv("/Users/k22129kk/Desktop/pre-research/csu_america.csv", header = TRUE, stringsAsFactors = TRUE)

# Q-Qプロット（全体確認）
qqnorm(data$score)
qqline(data$score, col = "red")

# シャピロ・ウィルク検定（全体）
shapiro.test(data$score)

# 各mapごとの正規性検定
by(data$score, data$Algorithm, shapiro.test)

# Levene検定（等分散性）
leveneTest(score ~ Algorithm, data = data)

# 分散分析の実行
anova_result <- aov(score ~ Algorithm, data = data)

# 分散分析の結果を表示
summary(anova_result)

# テューキー検定
tukey_result <- TukeyHSD(anova_result)

# テューキー検定の結果を表示
print(tukey_result)
