# 必要パッケージの読み込み
library(car)

# データの読み込み
data <- read.csv("/Users/k22129kk/Desktop/tmp/java/singapore.csv", header = TRUE, stringsAsFactors = TRUE)

# Q-Qプロット（全体確認）
qqnorm(data$score)
qqline(data$score, col = "red")

# シャピロ・ウィルク検定（全体）
shapiro.test(data$score)

# 各mapごとの正規性検定
by(data$score, data$Algorithm, shapiro.test)

# Levene検定（等分散性）
leveneTest(score ~ Algorithm, data = data)

# データの分布に問題があるので、分散分析（ANOVA）の代わりに**Kruskal-Wallis検定**を使用
kruskal.test(score ~ Algorithm, data = data)

# Kruskal-Wallisに有意差があった場合、**多重比較**（Dunn検定など）を行う
library(FSA)
options(scipen = 999)  # 科学的記数法を使わないようにする→小数点で出力される

# Dunn検定（p値の補正方法はBonferroni or Holmなど選択可能）
dunnTest(score ~ Algorithm, data = data, method = "bonferroni")
