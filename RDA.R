library(vegan)
library(readxl)

# 读取Excel文件（替换为实际文件路径）
file_path <- "C:/Users/leaderkk/Desktop/论文/根系-曹/数据/Rstudio/RDA.xlsx"  # 修改为你的Excel文件路径

# 如果数据在一个工作表中，按列分割：
all_data <- read_excel(file_path, sheet = "Sheet1")
species_data <- all_data[, 1:4]    # 假设前5列为物种数据
env_data <- all_data[, 5:13]        # 假设后3列为环境数据

# 执行RDA（假设环境变量为Temp、pH、Nutrient）
# 注意：变量名需与环境数据列名一致！
rda_result <- rda(species_data ~ SOM + TP + BD + EEGRSP + Clay + Silt + RLD + SPL + RMD, data = env_data)

# 全局模型显著性检验
global_test <- anova(rda_result, permutations = 999)
print("全局模型显著性:")
print(global_test)

# 各环境因子显著性检验（逐个项检验）
terms_test <- anova(rda_result, by = "terms", permutations = 999)
print("各环境因子显著性:")
print(terms_test)
# 提取方差解释量（R²和调整R²）
r2 <- RsquareAdj(rda_result)$r.squared
r2_adj <- RsquareAdj(rda_result)$adj.r.squared

# 提取各环境因子在排序轴上的贡献（载荷）
var_contrib <- summary(rda_result)$biplot  # 环境因子的排序轴载荷

# 整理显著性结果表格
signif_table <- data.frame(
  Term = rownames(terms_test$anova),
  F_value = terms_test$anova$F,
  P_value = terms_test$anova$`Pr(>F)`
)

# 输出结果
cat("\n=== 模型方差解释量 ===\n")
cat("原始R²:", r2, "\n")
cat("调整R²:", r2_adj, "\n\n")

cat("=== 各环境因子显著性 ===\n")
print(signif_table)

cat("\n=== 环境因子在排序轴上的贡献 ===\n")
print(var_contrib)

# 绘制RDA排序图
plot(rda_result, display = c("sites", "bp"), main = "RDA Triplot")
text(rda_result, display = "species", col = "blue", cex = 0.8)
