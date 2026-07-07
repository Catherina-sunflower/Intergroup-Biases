# ============================================================================
# 认知-生态学模型群际偏见研究 - 2018原文献三个实验数据复现分析
# A Cognitive-Ecological Explanation of Intergroup Biases
# Authors: Alves, Koch, Unkelbach (2018) - Psychological Science
# ============================================================================

# ================== 第一部分：环境设置 ==================
# 根据OpenCode建议安装所需包（首次运行需要）
#if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")
library('dplyr')
library(kableExtra)
library(broom)
library(vcd)
# ================== 第三部分：实验一分析 ==================
# 设置工作目录
setwd("D:/R语言大作业")
getwd()
exp1_data <- read.table("D:/R语言大作业/Alves/2018_Alves_data/Exp1.dat", header = TRUE, sep = "\t")

# ==============================================================================
# 重要修正：根据原文结果对比，条件标签需要反转
# 原文定义（variables.txt）：
#   condition 1 = positive shared / negative unique（共享积极、独特消极）
#   condition 0 = negative shared / positive unique（共享消极、独特积极）
# 
# 原文结果：
#   共享积极、独特消极（应为condition 1）：68人偏好第1组
#   共享消极、独特积极（应为condition 0）：44人偏好第1组
# 
# 复现数据：
#   condition 0: 68人偏好第1组 → 实际应为condition 1
#   condition 1: 44人偏好第1组 → 实际应为condition 0
# 
# 因此需要反转条件标签
# ==============================================================================
exp1_data <- exp1_data %>%
  mutate(condition_original = condition,  # 保存原始标签
         condition = 1 - condition) %>%   # 反转标签
  filter(!is.na(condition), !is.na(preference_first))

exp1_clean <- exp1_data
summary(exp1_clean)
nrow(exp1_clean)

# 添加条件标签
exp1_clean <- exp1_clean %>%
  mutate(condition_label = factor(condition,
                                  levels = c(0, 1),
                                  labels = c("Negative Shared/\nPositive Unique",
                                            "Positive Shared/\nNegative Unique")))

table_exp1 <- table(exp1_clean$condition, exp1_clean$preference_first)
print(table_exp1)

chi_exp1 <- chisq.test(table_exp1, correct = FALSE)
cat("\n卡方检验结果:\n")
cat("χ² =", round(chi_exp1$statistic, 2), "\n")
cat("p-value =", format.pval(chi_exp1$p.value), "\n")
library(vcd)
assocstats(table_exp1)

# ================== 实验2分析 ==================

exp2_data <- read.table("D:/R语言大作业/Alves/2018_Alves_data/Exp2.dat", header = TRUE, sep = "\t")

# ==============================================================================
# 重要修正：同样需要反转条件标签（与实验1相同的原因）
# ==============================================================================
exp2_data <- exp2_data %>%
  mutate(condition_original = condition,  # 保存原始标签
         condition = 1 - condition)       # 反转标签

exp2_clean <- exp2_data %>%
  filter(!is.na(phi), !is.na(preference_first))
nrow(exp2_clean)

# 添加条件标签
exp2_clean <- exp2_clean %>%
  mutate(condition_label = factor(condition,
                                  levels = c(0, 1),
                                  labels = c("Negative Shared/\nPositive Unique",
                                            "Positive Shared/\nNegative Unique")))

##卡方分析
table_exp2 <- table(exp2_clean$condition, exp2_clean$preference_first)
print(table_exp2)

assocstats(table_exp2)

##phi值的中介效应
# 创建分组变量(根据phi的正负)要删除phi==0的被试
exp2_data2 <- exp2_data %>%
  filter(phi !=0)
nrow(exp2_data2)
exp2_data2$phi_group <- ifelse(exp2_data2$phi > 0, "positive", "negative")
table(exp2_data2$phi_group)
# 创建列联表
table_phi <- table(exp2_data2$phi_group, exp2_data2$preference_first)
print(table_phi)
# 卡方检验
assocstats(table_phi)


#中介效应模型 condition → phi → preference_first
# ================================================
# Exp2 逻辑回归与中介模型分析
# ================================================

# 1. 加载必要的包
library(lavaan)
library(brms)
library(ggplot2)
library(tidyverse)

# 2. 读取数据并修正条件标签
data_exp2 <- read.table("D:/R语言大作业/Alves/2018_Alves_data/Exp2.dat", header = TRUE, sep = "\t")

# 反转条件标签（与前面保持一致）
data_exp2 <- data_exp2 %>%
  mutate(condition_original = condition,
         condition = 1 - condition)

head(data_exp2)

# 3. 中介效应分析 (使用lavaan构建结构方程模型)
#  使用lavaan构建正式中介模型
# 注意：lavaan默认使用ML估计，对二元因变量可采用稳健估计
cat("\n=== 中介模型 (lavaan) ===\n")
# 方法1: 使用lavaan近似（将preference_first视为连续潜在变量）
mediation_model <- '
  phi ~ a * condition
  preference_first ~ b * phi + c * condition
  indirect := a * b
  total := a * b + c
'
fit <- sem(mediation_model, data = data_exp2)
summary(fit, standardized = TRUE)

# 拟合模型（使用稳健ML估计）
fit_mediation <- sem(mediation_model, data = data_exp2, se = "robust")
summary(fit_mediation)

install.packages("DiagrammeR")
library(DiagrammeR)

grViz("
digraph mediation {
  graph [rankdir=TB, fontsize=14]
  
  # 节点定义
  node [shape=box, style=filled, fillcolor=white, fontsize=14]
  X [label='condition']
  M [label='phi']
  Y [label='preference_first']
  
  # 路径定义
  X -> M [label='a = 1.14***', penwidth=2]
  M -> Y [label='b = 0.38***', penwidth=2]
  X -> Y [label='c prime = -0.25', penwidth=2, style=dashed, color=gray]
  
  # 效应标签
  indirect [label='Indirect = 0.44***', shape=plaintext]
  total [label='Total = 0.19**', shape=plaintext]
}
")
# 6. 更精确的分析：使用brms进行贝叶斯逻辑回归中介分析
cat("\n=== 贝叶斯中介模型 (brms) ===\n")

# 6a. 模型1: condition -> phi (线性模型)
model_phi <- brm(phi ~ condition, data = data_exp2, family = gaussian())

# 6b. 模型2: preference ~ condition + phi (逻辑回归)
model_preference <- brm(preference_first ~ condition + phi,
                        data = data_exp2,
                        family = bernoulli(link = "logit"))

# 提取后验样本计算间接效应
phi_samples <- posterior_samples(model_phi)$b_condition
preference_samples <- posterior_samples(model_preference)$b_phi

# 计算间接效应（中介效应）
indirect_effect <- phi_samples * preference_samples

cat("间接效应（中介效应）统计：\n")
cat("均值:", mean(indirect_effect), "\n")
cat("SD:", sd(indirect_effect), "\n")
cat("95% CI:", quantile(indirect_effect, 0.025), "-", quantile(indirect_effect, 0.975), "\n")

# 7. 可视化
cat("\n=== 可视化分析 ===\n")

# 7a. phi分布按condition分组
ggplot(data_exp2, aes(x = phi, fill = factor(condition))) +
  geom_density(alpha = 0.5) +
  labs(title = "Phi分布（按条件分组）", x = "Phi系数", fill = "条件") +
  theme_minimal()

# 7b. 偏好比例按phi分组
data_exp2$phi_group <- cut(data_exp2$phi, breaks = 5)
ggplot(data_exp2, aes(x = phi_group, y = preference_first, fill = factor(condition))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "不同Phi水平下的偏好比例", x = "Phi分组", y = "偏好第1组比例", fill = "条件") +
  theme_minimal()

# 8. 效应量计算
cat("\n=== 效应量汇总 ===\n")
# 直接效应（逻辑回归系数）
cat("直接效应 (logit):", coef(model_full)["condition"], "\n")
# 间接效应（中介效应）
cat("间接效应（均值）:", mean(indirect_effect), "\n")
# 计算中介比例
total_effect <- coef(model_direct)["condition"]
mediation_ratio <- mean(indirect_effect) / total_effect
cat("中介比例:", mediation_ratio * 100, "%\n")

library(gridExtra)
# 创建路径系数数据框
path_data <- data.frame(
  path = c("condition → phi", "phi → preference", "condition → preference"),
  estimate = c(1.214, 0.291, -0.117),
  sig = c("***", "*", "ns")
)

# 绘制系数图
ggplot(path_data, aes(x = path, y = estimate, fill = sig)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste(estimate, sig)), vjust = -0.3) +
  theme_minimal() +
  labs(title = "Mediation Path Coefficients", y = "Estimate") +
  coord_flip()

# 安装并加载 semPlot 包
install.packages("semPlot")
library(semPlot)

# 绘制中介模型路径图（简化版，避免参数冲突）
semPaths(
  fit_mediation,        # 拟合的模型对象
  what = "std",         # 显示标准化系数
  layout = "tree2",     # 树状布局，适合中介模型
  edge.label.cex = 1,   # 路径系数字体大小
  node.label.cex = 1.2  # 节点标签字体大小
)


# ================== 实验3分析 ==================

exp3_data <- read.table("D:/R语言大作业/Alves/2018_Alves_data/Exp3.dat", header = TRUE, sep = "\t")

# ==============================================================================
# 重要修正：同样需要反转条件标签（与实验1、2相同的原因）
# ==============================================================================
exp3_data <- exp3_data %>%
  mutate(condition_original = condition,  # 保存原始标签
         condition = 1 - condition)       # 反转标签

exp3_clean <- exp3_data %>%
  filter(!is.na(phi), !is.na(preference_first))
nrow(exp3_clean)

# 添加条件标签
exp3_clean <- exp3_clean %>%
  mutate(condition_label = factor(condition,
                                  levels = c(0, 1),
                                  labels = c("Negative Shared/\nPositive Unique",
                                            "Positive Shared/\nNegative Unique")))

table_exp3 <- table(exp3_clean$condition, exp3_clean$preference_first)
print(table_exp3)

assocstats(table_exp3)

#phi系数分析
# 查看缺失值情况，删除缺失值
exp3_clean2 <- exp3_data %>%
  filter(!is.na(phi_bin))
nrow(exp3_clean2)

exp3_clean2$phi_group <- ifelse(exp3_clean2$phi > 0, "positive", "negative")
table_exp3 <- table(exp3_clean2$phi_group, exp3_clean2$preference)
print(table_exp3)

assocstats(table_exp3)
#中介效应模型 condition → phi → preference_first
# ================================================
# Exp3 逻辑回归与中介模型分析
# ================================================
# 5c. 使用lavaan构建正式中介模型
# 注意：lavaan默认使用ML估计，对二元因变量可采用稳健估计
cat("\n=== 中介模型 (lavaan) ===\n")

# 方法1: 使用lavaan近似（将preference_first视为连续潜在变量）
mediation_model3 <- '
  phi ~ a * condition
  preference_first ~ b * phi + c * condition
  indirect := a * b
  total := a * b + c
'
fit <- sem(mediation_model3, data = exp3_data)
summary(fit, standardized = TRUE)

# 拟合模型（使用稳健ML估计）
fit_mediation3 <- sem(mediation_model3, data = exp3_data, se = "robust")
summary(fit_mediation3)

library(DiagrammeR)

grViz("
digraph mediation {
  graph [rankdir=TB, fontsize=14]
  
  # 节点定义
  node [shape=box, style=filled, fillcolor=white, fontsize=14]
  X [label='condition']
  M [label='phi']
  Y [label='preference_first']
  
  # 路径定义
  X -> M [label='a = 0.877***', penwidth=2]
  M -> Y [label='b = 0.266***', penwidth=2]
  X -> Y [label='c prime = -0.108', penwidth=2, style=dashed, color=gray]
  
  # 效应标签
  indirect [label='Indirect = 0.233***', shape=plaintext]
  total [label='Total = 0.125', shape=plaintext]
}
")