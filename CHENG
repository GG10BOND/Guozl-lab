if (!require(mypackage)) {
  install.packages("mypackage")
  library(mypackage)
}
library(readxl)
library(corrplot)
library(psych)
library(ggplot2)
# 1. 参数设置 ----------------------------------------------------------------
excel_path <- "C:/Users/leaderkk/Desktop/论文/根系-曹/数据/Rstudio/RDA.xlsx"
sheet_name <- "Sheet1"

# 定义列分组（按实际Excel列名修改）
soil_vars <- c("SOM", "TP", "BD", "EEGRSP", "RLD", "Clay", "Silt", "SPL","RMD")
env_vars <- c("Cenotype", "Soil_depth")

# 2. 数据读取与处理 ----------------------------------------------------------
df <- read_excel(excel_path, sheet = sheet_name)
df <- df[, c(soil_vars, env_vars)]
df <- data.frame(lapply(df, as.numeric))
df <- na.omit(df)

# 3. 计算相关系数与p值矩阵 --------------------------------------------------
cor_results <- corr.test(
  x = df[, soil_vars], 
  y = df[, env_vars],
  method = "pearson",
  adjust = "none"
)

cor_matrix <- cor_results$r  # 相关系数矩阵
p_matrix <- cor_results$p    # p值矩阵

# 4. 可视化生成 ------------------------------------------------------------
png("final_plot3.png", width = 1200, height = 2000, res = 300)
par(mar = c(4, 8, 4, 4))

# 自定义颜色
col <- colorRampPalette(c("#053061", "white", "#67001F"))(100)

# 自定义缩放函数
col_scale <- function(x) {
  # 将矩阵的值缩放到 1 到 100 的范围
  scaled <- (x - min(x)) / (max(x) - min(x)) * 99 + 1
  return(scaled)
}

# 手动调整颜色映射
col_matrix <- matrix(col[round(col_scale(cor_matrix))], nrow = nrow(cor_matrix))

library(corrplot)
corrplot(cor_matrix,
         method = "color",
         is.corr = FALSE,
         col = col,
         cl.ratio = 0.6,      # 颜色标尺高度比例
         tl.col = "black",    # 行标签颜色
         tl.cex = 0.5,        # 行标签字号
         tl.srt = 0,          # 行标签不旋转
         p.mat = p_matrix,
         sig.level = c(0.05, 0.01),
         insig = "blank", # 先不显示默认的显著性标记
         mar = c(0, 0, 2, 0)) # 边距调整

# 手动添加显著性 p 值
for(i in 1:nrow(p_matrix)){
  for(j in 1:ncol(p_matrix)){
    text(j, i, labels = format(p_matrix[i,j], digits = 2), cex = 0.3)
  }
}

dev.off()
