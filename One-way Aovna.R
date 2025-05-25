library(readxl)
library(dplyr)

# 读取Excel文件（假设文件名为data.xlsx）
data <- read_excel("C:/Users/ZJK/Desktop/Rwork.xlsx")


# 查看数据结构
str(data)
library(car)

# 确保分组变量是因子
data$month<- as.factor(data$month)

# 单因素方差分析
model <- aov(area ~ month , data = data)

# 方差齐性检验（Levene Test）
leveneTest(area ~ month, data = data)

# 查看ANOVA结果
summary(model)

# 事后检验（Tukey HSD）
TukeyHSD(model)

# 绘制箱线图
library(ggplot2)
ggplot(data, aes(x = month, y = area, fill = month)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Group Comparison (ANOVA)") 
#上述代码无问题
#绘制柱状图部分
str(month)  # 查看data的结构和类型
num <- 123
char <- as.character(num)
print(char)  # 输出: "123"




# 提取 Group 部分的 p 值矩阵
tukey_p <- tukey_result$month[, "p adj"]

# 生成显著性字母
group_letters <- multcompLetters(tukey_p)

# 合并字母到数据中
letters_df <- data.frame(
  Group = names(group_letters$Letters),
  Letter = group_letters$Letters
)

# 计算均值和标准误
plot_data <- data %>%
  group_by(month) %>%
  summarise(
    Mean = mean(area, na.rm = TRUE),
    SE = sd(area, na.rm = TRUE) / sqrt(n())
  ) %>%
  left_join(letters_df, by = "month")

# 绘制柱状图
ggplot(plot_data, aes(x = month, y = Mean, fill = month)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  geom_text(aes(y = Mean + SE, label = Letter), vjust = -0.5, size = 5) +
  labs(x = "处理组", y = "测量值", title = "均值比较（不同字母表示显著差异）") +
  theme_classic()