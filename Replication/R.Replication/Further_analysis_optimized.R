# ==============================================================================
# Alves et al. (2018) 进一步分析代码 - 优化版
# 基于原始研究报告精简优化
# ==============================================================================

setwd("D:/R语言大作业/Replication")

# 加载必要包
library(dplyr)

# ---------------------------
# 1. 数据导入与预处理
# ---------------------------
dat <- read.csv("Replication_data/Alves_PsychologSci_2018_AvOr-Gleibs_92g_raw.csv", 
                stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  # 移除无关列
  select(-c(Progress, `Duration..in.seconds.`, Finished, RecordedDate, 
            DistributionChannel, UserLanguage, PROLIFIC_PID, 
            gender_3_TEXT, ethnicity_10_TEXT, comments)) %>%
  # 移除第一行说明
  slice(-1) %>%
  # 过滤有效数据
  filter(tribe_preference != "" & !is.na(tribe_preference)) %>%
  # 变量类型转换
  mutate(
    condition = as.numeric(condition),
    tribe_preference = as.numeric(tribe_preference),
    age = as.numeric(age)
  )

# ---------------------------
# 2. 变量重编码
# ---------------------------
dat <- dat %>%
  mutate(
    # 条件分组: SP=共享正面(1,3), SN=共享负面(2,4)
    condition_rec = ifelse(condition %in% c(1, 3), "SP", "SN"),
    # 部落偏好重编码: 0=选第一个呈现的部落, 1=选第二个
    tribe_preference_rec = ifelse(condition %in% c(1, 2), tribe_preference, 1 - tribe_preference),
    # 注意力检查: 通过=1, 未通过=0
    attention_pass = ifelse(grepl("pilut", tolower(attention_check)) | 
                           tolower(attention_check) == "pilot" | 
                           attention_check == "Magnolia", 1, 0)
  )

# ---------------------------
# 3. 分析1: 排除注意力检查失败后的焦点检验
# ---------------------------
cat("=== 分析1：排除未通过注意力检查者后的焦点检验 ===\n")
dat_exclude <- dat %>% filter(attention_pass == 1)
cat("排除后样本量: N =", nrow(dat_exclude), "\n")

# 卡方检验
myTable <- table(dat_exclude$condition_rec, dat_exclude$tribe_preference_rec)
cat("\n列联表:\n")
print(myTable)

chi_result <- chisq.test(myTable, correct = FALSE)
cat("\n卡方检验结果:\n")
cat(sprintf("χ²(%d, N=%d) = %.3f, p = %s\n", 
            chi_result$parameter, sum(myTable),
            chi_result$statistic, format(chi_result$p.value, sci=T, dig=3)))
cat(sprintf("Cohen's w = %.4f\n", sqrt(unname(chi_result$statistic)/sum(myTable))))

# ---------------------------
# 4. 分析2: 性别和年龄对主要结果的影响
# ---------------------------
cat("\n=== 分析2：性别和年龄对主要结果的影响 ===\n")

# 描述统计
cat("\n【性别分布】\n")
gender_counts <- table(dat$gender)
print(gender_counts)
cat(sprintf("女性比例: %.1f%%, 男性比例: %.1f%%\n",
            sum(dat$gender == "1")/nrow(dat)*100,
            sum(dat$gender == "2")/nrow(dat)*100))

cat("\n【年龄统计】\n")
cat(sprintf("年龄范围: %d-%d岁, 平均年龄: %.1f岁 (SD=%.1f)\n",
            min(dat$age, na.rm=T), max(dat$age, na.rm=T),
            mean(dat$age, na.rm=T), sd(dat$age, na.rm=T)))

# Logistic回归
cat("\n【Logistic回归结果】\n")
dat_glm <- dat %>% filter(!is.na(gender) & !is.na(age) & !is.na(tribe_preference_rec))
glm_model <- glm(tribe_preference_rec ~ condition_rec + gender + age, 
                 data = dat_glm, family = binomial)

coef_table <- summary(glm_model)$coefficients
rownames(coef_table) <- c("截距", "条件(SP)", "性别(男)", "年龄")
print(round(coef_table[, c("Estimate", "z value", "Pr(>|z|)")], 3))

# ---------------------------
# 5. 保存结果
# ---------------------------
sink("Further_analysis_results.txt")
cat("=== Alves et al. (2018) 进一步分析结果 ===\n")
cat("日期:", format(Sys.Date(), "%Y-%m-%d"), "\n\n")

cat("【排除注意力检查失败后的焦点检验】\n")
cat(sprintf("样本量: N=%d\n", nrow(dat_exclude)))
print(myTable)
cat(sprintf("\nχ²(%d, N=%d) = %.3f, p = %s\n", 
            chi_result$parameter, sum(myTable),
            chi_result$statistic, format(chi_result$p.value, sci=T, dig=3)))
cat(sprintf("Cohen's w = %.4f\n\n", sqrt(unname(chi_result$statistic)/sum(myTable))))

cat("【性别和年龄效应】\n")
cat("Logistic回归结果:\n")
print(round(coef_table[, c("Estimate", "z value", "Pr(>|z|)")], 3))
sink()

cat("\n✅ 分析完成！结果已保存到 Further_analysis_results.txt\n")