# ==============================================================================
# Alves et al. (2018) 数据复现代码
# 研究：共享特质与独特特质对群体偏好的影响
# ==============================================================================

setwd("D:/R语言大作业/Replication")

# ---------------------------
# 1. 数据导入与预处理
# ---------------------------
# 读取原始数据
raw_data <- read.csv("D:/R语言大作业/Replication/Replication_data/Alves_PsychologSci_2018_AvOr-Gleibs_92g_raw.csv", 
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

# 将关键变量转换为数值类型
dat$condition <- as.numeric(dat$condition)
dat$tribe_preference <- as.numeric(dat$tribe_preference)

# ---------------------------
# 2. 变量重编码
# ---------------------------

# 2.1 条件分组：1&3=SP(共享积极), 2&4=SN(共享消极)
dat$condition_rec <- ifelse(dat$condition %in% c(1, 3), "SP", "SN")

# 2.2 部落偏好重编码
# 原始编码: 0 = Purple Alien A, 1 = Brown Alien B
# 重编码规则: 0 = 选择第一个呈现的部落, 1 = 选择第二个呈现的部落
# - 条件1和2：Purple A先呈现，所以0=选A(第一个), 1=选B(第二个)
# - 条件3和4：Brown B先呈现，所以1=选B(第一个), 0=选A(第二个)→需要反转

dat$tribe_preference_rec <- NA

# 条件1和2：A先呈现，直接使用原始值
mask_12 <- dat$condition %in% c(1, 2)
dat$tribe_preference_rec[mask_12] <- dat$tribe_preference[mask_12]

# 条件3和4：B先呈现，需要反转
mask_34 <- dat$condition %in% c(3, 4)
dat$tribe_preference_rec[mask_34] <- 1 - dat$tribe_preference[mask_34]

# ---------------------------
# 3. 描述性统计与可视化
# ---------------------------

cat("\n=== 各条件下的偏好分布 ===\n")
cat("条件1 (SP - A先呈现):\n")
print(table(dat$tribe_preference_rec[dat$condition == 1]))

cat("\n条件2 (SN - A先呈现):\n")
print(table(dat$tribe_preference_rec[dat$condition == 2]))

cat("\n条件3 (SP - B先呈现):\n")
print(table(dat$tribe_preference_rec[dat$condition == 3]))

cat("\n条件4 (SN - B先呈现):\n")
print(table(dat$tribe_preference_rec[dat$condition == 4]))

# 可视化各条件下的偏好分布
library(ggplot2)

# 创建可视化数据
plot_data <- dat %>%
  mutate(
    condition_label = case_when(
      condition == 1 ~ "1. SP - A先呈现",
      condition == 2 ~ "2. SN - A先呈现",
      condition == 3 ~ "3. SP - B先呈现",
      condition == 4 ~ "4. SN - B先呈现"
    ),
    preference_label = ifelse(tribe_preference_rec == 0, "选择第一个部落", "选择第二个部落")
  ) %>%
  group_by(condition_label, preference_label) %>%
  summarise(count = n()) %>%
  group_by(condition_label) %>%
  mutate(percentage = count / sum(count) * 100)

# 创建可视化图表
condition_plot <- ggplot(plot_data, aes(x = condition_label, y = percentage, fill = preference_label)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7, color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")), 
            position = position_fill(vjust = 0.5), 
            size = 3.5, color = "white", fontface = "bold") +
  labs(
    title = "各条件下的部落偏好分布",
    subtitle = "N = " %+% nrow(dat),
    x = "实验条件",
    y = "比例",
    fill = "选择偏好"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# 保存可视化图表
ggsave("condition_preference_plot.png", condition_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("\n各条件偏好分布图已保存到: condition_preference_plot.png\n")

# ---------------------------
# 4. 卡方检验（核心分析）
# ---------------------------

cat("\n=== 核心分析：2×2卡方检验 ===\n")

# 构建列联表
myTable <- table(dat$condition_rec, dat$tribe_preference_rec)
cat("列联表:\n")
print(myTable)

cat("\n行百分比:\n")
print(round(prop.table(myTable, 1) * 100, 2))

# 卡方检验（不使用连续性校正，与原始研究一致）
chi_result <- chisq.test(myTable, correct = FALSE)

cat("\n卡方检验结果:\n")
cat("Chi-square:", round(chi_result$statistic, 3), "\n")
cat("df:", chi_result$parameter, "\n")
cat("p-value:", format(chi_result$p.value, scientific = TRUE, digits = 3), "\n")
cat("N:", sum(myTable), "\n")

# 效应量计算（Cohen's w）
n <- sum(myTable)
chi2 <- unname(chi_result$statistic)
cohens_w <- sqrt(chi2 / n)
cat("\n效应量:\n")
cat("Cohen's w:", round(cohens_w, 5), "\n")


#EXPLORATORY ANALYSIS (Optional)
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