# =============================================================================
# Alves et al. (2018) 数据复现代码
# 复现研究: Ilka H. Gleibs, Nihan Albayrak-Aydemir (LSE)
# 项目ID: Alves_PsychologSci_2018_AvOr-Gleibs_92g
# =============================================================================

# -----------------------------------------------------------------------------
# Step 1: 数据导入与清洗
# -----------------------------------------------------------------------------

# 设置工作目录（请根据实际路径修改）
setwd("D:/R语言大作业/Replication")

# 读取原始数据
raw_data <- read.csv("Alves_PsychologSci_2018_AvOr-Gleibs_92g_raw.csv", 
                     stringsAsFactors = FALSE, encoding = "UTF-8")

cat("原始数据维度:", nrow(raw_data), "行 x", ncol(raw_data), "列\n")

# 删除可识别信息列和系统列
cols_to_remove <- c("StartDate", "EndDate", "Status", "IPAddress",
                    "RecordedDate", "DistributionChannel", "UserLanguage",
                    "PROLIFIC_PID", "Finished","gender_3_TEXT","ethnicity_10_TEXT","comments")
data <- raw_data[, !names(raw_data) %in% cols_to_remove]

# 删除第一行（变量说明行）
data <- data[-1, ]
# 删除部落偏好为空的行
data <- subset(data, tribe_preference != "" & !is.na(tribe_preference))

cat("删除反应时和空偏好后维度:", nrow(data), "行 x", ncol(data), "列\n")

library(dplyr)
# 只保留指定的列
dat <- data %>% select(condition, tribe_preference)
cat("用于复现实验一的数据维度:", nrow(dat), "行 x", ncol(dat), "列\n")


# -----------------------------------------------------------------------------
# Step 2: 变量重编码
# -----------------------------------------------------------------------------

# 2.1 条件变量重编码为因子
factor(dat$condition)
#条件1. 先展示部落A（3积极+3消极），再展示部落B：3积极特征与A相同，3消极特征与A不同	（共享积极）预期：0多 1少
#条件2. 先展示部落A（3积极+3消极），再展示部落B：3积极特征与A不同，3消极特征与A相同	（共享消极）预期：0少 1多
#条件3. 先展示部落B（3积极+3消极），再展示部落A：3积极特征与B相同，3消极特征与B不同	（共享积极）预期：0多 1少
#条件4. 先展示部落B（3积极+3消极），再展示部落A：3积极特征与B不同，3消极特征与B相同	（共享消极）预期：0少 1多

cat("\n各条件频数分布:\n")
print(table(dat$condition))

# 将条件重编码为二分类
dat$condition_rec <- ifelse(dat$condition %in% c(1, 3), 
                            "SharedPositive", "SharedNegative")

# 构建2×2列联表
myTable <- table(dat$condition_rec, dat$tribe_preference_rec)

# 卡方检验
chisq.test(myTable, correct = FALSE)  # 此时 df = 1
cat("\n核心分析交叉表 (N =", nrow(dat), "):\n")
myTable <- table(dat$condition, dat$tribe_preference)
print(myTable)


# 构建2×2列联表
myTable <- table(dat$condition, dat$tribe_preference)

# 卡方检验
chisq.test(myTable, correct = FALSE)  # 此时 df = 1
# 4.3 百分比
cat("\n行百分比:\n")
print(prop.table(myTable, 1) * 100)
# 5.1 卡方检验（不使用连续性校正）
chi_result <- chisq.test(myTable, correct = FALSE)

cat("\n卡方检验结果:\n")
cat("Chi-square:", round(chi_result$statistic, 3), "\n")
cat("df:", chi_result$parameter, "\n")
cat("p-value:", format(chi_result$p.value, scientific = TRUE, digits = 3), "\n")
cat("N:", sum(myTable), "\n")


# 5.2 效应量计算

# Cohen's w
n <- sum(myTable)
chi2 <- unname(chi_result$statistic)
cohens_w <- sqrt(chi2 / n)
cat("\nCohen's w:", round(cohens_w, 5), "\n")