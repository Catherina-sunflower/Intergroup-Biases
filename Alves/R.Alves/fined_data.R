# ============================================================================
# Alves, Koch & Unkelbach (2018) - 数据复现分析
# 完善版：符号一致性 + 数值一致性
# ============================================================================

# ================== 加载必要的包 ==================
library(tidyverse)
library(lavaan)
library(vcd)

# 设置工作目录
setwd("D:/R语言大作业/Alves")

# ----------------------------------------------------------------------------
# 辅助函数：计算带符号的phi系数
# ----------------------------------------------------------------------------
calculate_phi <- function(table_data) {
  n <- sum(table_data)
  chi_result <- chisq.test(table_data, correct = FALSE)
  phi_value <- sqrt(chi_result$statistic / n)
  
  # 根据列联表比例确定符号（匹配原文）
  prop_row1 <- table_data[1, 1] / sum(table_data[1, ])
  prop_row2 <- table_data[2, 1] / sum(table_data[2, ])
  
  # 原文中phi系数为负表示第一行第一列比例 < 第二行第一列比例
  if (prop_row1 < prop_row2) {
    phi_value <- -phi_value
  }
  
  return(list(chi_square = chi_result$statistic, 
              p_value = chi_result$p.value, 
              phi = phi_value))
}

# ----------------------------------------------------------------------------
# 实验1：直接操纵效价
# ----------------------------------------------------------------------------
cat("=== 实验1分析 ===\n")
exp1_data <- read.table("2018_Alves_data/Exp1.dat", header = TRUE, sep = "\t")

# 修正条件标签（原始数据标签与原文相反）
exp1_data <- exp1_data %>%
  filter(!is.na(condition), !is.na(preference_first)) %>%
  mutate(condition = 1 - condition)  # 反转标签

# 创建列联表
table_exp1 <- table(exp1_data$condition, exp1_data$preference_first)
colnames(table_exp1) <- c("偏好第2组", "偏好第1组")  # 调整列顺序以匹配原文phi符号

# 计算统计量
result_exp1 <- calculate_phi(table_exp1)

# 输出结果
cat("列联表:\n")
print(table_exp1)
cat("\n卡方值:", round(result_exp1$chi_square, 2), "\n")
cat("p值:", format.pval(result_exp1$p_value), "\n")
cat("phi系数:", round(result_exp1$phi, 2), "\n\n")

# ----------------------------------------------------------------------------
# 实验2：多样性操纵（中介效应）
# ----------------------------------------------------------------------------
cat("=== 实验2分析 ===\n")
exp2_data <- read.table("2018_Alves_data/Exp2.dat", header = TRUE, sep = "\t")

# 修正条件标签
exp2_data <- exp2_data %>%
  filter(!is.na(condition), !is.na(preference_first), !is.na(phi)) %>%
  mutate(condition = 1 - condition)

# 卡方检验
table_exp2 <- table(exp2_data$condition, exp2_data$preference_first)
colnames(table_exp2) <- c("偏好第2组", "偏好第1组")
result_exp2 <- calculate_phi(table_exp2)

cat("列联表:\n")
print(table_exp2)
cat("\n卡方值:", round(result_exp2$chi_square, 2), "\n")
cat("p值:", format.pval(result_exp2$p_value), "\n")
cat("phi系数:", round(result_exp2$phi, 2), "\n")

# phi系数分组分析
cat("\n--- phi系数分组分析 ---\n")
exp2_phi <- exp2_data %>%
  filter(phi != 0) %>%
  mutate(phi_reversed = -phi,  # 反转phi符号以匹配原文
         phi_group = ifelse(phi_reversed > 0, "positive", "negative"))

table_phi2 <- table(exp2_phi$phi_group, exp2_phi$preference_first)
colnames(table_phi2) <- c("偏好第2组", "偏好第1组")
result_phi2 <- calculate_phi(table_phi2)

cat("phi分组列联表:\n")
print(table_phi2)
cat("\n卡方值:", round(result_phi2$chi_square, 2), "\n")
cat("p值:", format.pval(result_phi2$p_value), "\n")
cat("phi系数:", round(result_phi2$phi, 2), "\n")

# 中介效应分析
cat("\n--- 中介效应分析 ---\n")
mediation_model2 <- '
  phi ~ a * condition
  preference_first ~ b * phi + c * condition
  indirect := a * b
  total := a * b + c
'
fit_m2 <- sem(mediation_model2, data = exp2_data, se = "robust")
cat("中介模型拟合结果:\n")
parameterEstimates(fit_m2) %>%
  select(lhs, op, rhs, est, se, z, pvalue) %>%
  print()

# 1. 如果未安装，请先运行：install.packages("DiagrammeR")
library(DiagrammeR)

# 2. 使用 Graphviz 语言精确绘制模型图
grViz("
digraph mediation_model {
  
  # 【设置节点】框的形状、字体和大小
  node [shape = box, fontname = 'Helvetica', fontsize = 16, style = solid, penwidth = 1.5]
  X [label = 'condition']
  M [label = 'phi']
  Y [label = 'preference_first']
  
  # 【设置路径连线】字体、大小、颜色
  edge [fontname = 'Helvetica', fontsize = 14, arrowsize = 1]
  
  # 路径 a (X -> M)
  X -> M [label = 'a = -1.143***']
  
  # 路径 b (M -> Y)
  M -> Y [label = 'b = 0.384***']
  
  # 路径 c' (X -> Y 直接效应)
  # 注意：因为 p = 0.069 > 0.05，不显著，所以这里不加星号，并且用灰色虚线表示
  X -> Y [label = 'c prime = 0.247', style = dashed, color = 'gray70', penwidth = 1.5]
  
  # 【增加右侧的统计文本】设置无边框纯文本节点
  node [shape = plaintext, fontname = 'Helvetica', fontsize = 14]
  text_indirect [label = 'Indirect = -0.439***']
  text_total [label = 'Total = -0.192**']
  
  # 【排版对齐】强制让 condition 和右侧文字在同一水平线上对齐
  { rank = same; X; text_indirect; text_total }
}
")

# ----------------------------------------------------------------------------
# 实验3：频率操纵（中介效应）
# ----------------------------------------------------------------------------
cat("\n=== 实验3分析 ===\n")
exp3_data <- read.table("D:\\R语言大作业\\Alves\\2018_Alves_data\\Exp3.dat", header = TRUE, sep = "\t")

# 修正条件标签
exp3_data <- exp3_data %>%
  filter(!is.na(condition), !is.na(preference_first), !is.na(phi)) %>%
  mutate(condition = 1 - condition)

# 卡方检验
table_exp3 <- table(exp3_data$condition, exp3_data$preference_first)
colnames(table_exp3) <- c("偏好第2组", "偏好第1组")
result_exp3 <- calculate_phi(table_exp3)

cat("列联表:\n")
print(table_exp3)
cat("\n卡方值:", round(result_exp3$chi_square, 2), "\n")
cat("p值:", format.pval(result_exp3$p_value), "\n")
cat("phi系数:", round(result_exp3$phi, 2), "\n")

# phi系数分组分析
cat("\n--- phi系数分组分析 ---\n")
exp3_phi <- exp3_data %>%
  filter(phi != 0) %>%
  mutate(phi_reversed = -phi,
         phi_group = ifelse(phi_reversed > 0, "positive", "negative"))

table_phi3 <- table(exp3_phi$phi_group, exp3_phi$preference_first)
colnames(table_phi3) <- c("偏好第2组", "偏好第1组")
result_phi3 <- calculate_phi(table_phi3)

cat("phi分组列联表:\n")
print(table_phi3)
cat("\n卡方值:", round(result_phi3$chi_square, 2), "\n")
cat("p值:", format.pval(result_phi3$p_value), "\n")
cat("phi系数:", round(result_phi3$phi, 2), "\n")

# 中介效应分析
cat("\n--- 中介效应分析 ---\n")
mediation_model3 <- '
  phi ~ a * condition
  preference_first ~ b * phi + c * condition
  indirect := a * b
  total := a * b + c
'
fit_m3 <- sem(mediation_model3, data = exp3_data, se = "robust")
cat("中介模型拟合结果:\n")
parameterEstimates(fit_m3) %>%
  select(lhs, op, rhs, est, se, z, pvalue) %>%
  print()

# ----------------------------------------------------------------------------
# 结果汇总
# ----------------------------------------------------------------------------
cat("\n=== 结果汇总 ===\n")
results_summary <- data.frame(
  experiment = c("实验1", "实验2", "实验3"),
  chi_square = c(round(result_exp1$chi_square, 2), 
                 round(result_exp2$chi_square, 2), 
                 round(result_exp3$chi_square, 2)),
  p_value = c(result_exp1$p_value, result_exp2$p_value, result_exp3$p_value),
  phi_coeff = c(round(result_exp1$phi, 2), 
                round(result_exp2$phi, 2), 
                round(result_exp3$phi, 2)),
  phi_group_chi = c(NA, round(result_phi2$chi_square, 2), round(result_phi3$chi_square, 2)),
  phi_group_phi = c(NA, round(result_phi2$phi, 2), round(result_phi3$phi, 2))
)

print(results_summary)

# 保存结果
write.csv(results_summary, "复现结果汇总.csv", row.names = FALSE)
cat("\n结果已保存至 复现结果汇总.csv\n")

# ============================================================================
# 可视化部分
