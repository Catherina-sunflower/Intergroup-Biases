# ==============================================================================
# Alves et al. (2018) 数据复现 - 可视化代码
# 创建美观的统计结果可视化图表
# ==============================================================================

setwd("D:/R语言大作业/Replication")

library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)
library(grid)

# ---------------------------
# 1. 数据导入与预处理
# ---------------------------
raw_data <- read.csv("Alves_PsychologSci_2018_AvOr-Gleibs_92g_raw.csv", 
                     stringsAsFactors = FALSE, encoding = "UTF-8")

cols_to_remove <- c("StartDate", "EndDate", "Status", "IPAddress",
                    "RecordedDate", "DistributionChannel", "UserLanguage",
                    "PROLIFIC_PID", "Finished","gender_3_TEXT","ethnicity_10_TEXT","comments")
data <- raw_data[, !names(raw_data) %in% cols_to_remove]

data <- data[-1, ]
data <- subset(data, tribe_preference != "" & !is.na(tribe_preference))

dat <- data %>% select(condition, tribe_preference)
dat$condition <- as.numeric(dat$condition)
dat$tribe_preference <- as.numeric(dat$tribe_preference)

# ---------------------------
# 2. 变量重编码
# ---------------------------
dat$condition_rec <- ifelse(dat$condition %in% c(1, 3), "SP", "SN")
dat$condition_label <- ifelse(dat$condition %in% c(1, 3), "共享正面情绪", "共享负面情绪")

dat$tribe_preference_rec <- NA
mask_12 <- dat$condition %in% c(1, 2)
dat$tribe_preference_rec[mask_12] <- dat$tribe_preference[mask_12]
mask_34 <- dat$condition %in% c(3, 4)
dat$tribe_preference_rec[mask_34] <- 1 - dat$tribe_preference[mask_34]

dat$preference_label <- ifelse(dat$tribe_preference_rec == 0, "选择第一个部落", "选择第二个部落")

# ---------------------------
# 3. 创建可视化数据
# ---------------------------
# 列联表数据
table_data <- dat %>%
  group_by(condition_label, preference_label) %>%
  summarise(count = n()) %>%
  group_by(condition_label) %>%
  mutate(percentage = count / sum(count) * 100)

# 卡方检验结果
myTable <- table(dat$condition_rec, dat$tribe_preference_rec)
chi_result <- chisq.test(myTable, correct = FALSE)
cohens_w <- sqrt(unname(chi_result$statistic) / sum(myTable))

# ---------------------------
# 4. 创建可视化图表
# ---------------------------

# 主题设置
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", hjust = 0.5),
    axis.title = element_text(size = 12, color = "#2c3e50"),
    axis.text = element_text(size = 10, color = "#34495e"),
    legend.title = element_text(size = 11, color = "#2c3e50"),
    legend.text = element_text(size = 10, color = "#34495e"),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 11, face = "bold", color = "#2c3e50")
  )

# 图表1：分组条形图
plot1 <- ggplot(table_data, aes(x = condition_label, y = percentage, fill = preference_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, 
           color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3.5, color = "#2c3e50") +
  labs(
    title = "部落偏好分布",
    subtitle = "不同情绪共享条件下的选择比例",
    x = "实验条件",
    y = "百分比 (%)",
    fill = "选择偏好"
  ) +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  my_theme

# 图表2：堆叠条形图
plot2 <- ggplot(table_data, aes(x = condition_label, y = count, fill = preference_label)) +
  geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.5) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), 
            size = 4, color = "white", fontface = "bold") +
  labs(
    title = "选择人数分布",
    subtitle = "各条件下的绝对频数",
    x = "实验条件",
    y = "人数",
    fill = "选择偏好"
  ) +
  scale_fill_manual(values = c("#2ecc71", "#9b59b6")) +
  my_theme

# 图表3：效应量展示
effect_data <- data.frame(
  measure = c("卡方值", "自由度", "p值", "Cohen's w"),
  value = c(round(unname(chi_result$statistic), 3), 
            chi_result$parameter,
            format(chi_result$p.value, scientific = TRUE, digits = 3),
            round(cohens_w, 4)),
  significance = c("**", "", "***", "**")
)

plot3 <- ggplot(effect_data, aes(x = measure, y = 1, label = value)) +
  geom_text(size = 4, color = "#2c3e50", fontface = "bold") +
  geom_text(aes(label = significance), y = 0.9, size = 5, color = "#e74c3c") +
  labs(
    title = "统计结果汇总",
    subtitle = paste0("N = ", sum(myTable))
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", hjust = 0.5)
  )

# 图表4：条件对比点图
point_data <- table_data %>%
  filter(preference_label == "选择第一个部落")

plot4 <- ggplot(point_data, aes(x = condition_label, y = percentage)) +
  geom_point(size = 8, color = "#3498db", fill = "#3498db", shape = 21, stroke = 2) +
  geom_line(aes(group = 1), color = "#3498db", linewidth = 2, linetype = "dashed") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -1.5, 
            size = 4, color = "#2c3e50", fontface = "bold") +
  labs(
    title = "选择第一个部落的比例",
    subtitle = "SP vs SN 条件对比",
    x = "实验条件",
    y = "百分比 (%)"
  ) +
  scale_y_continuous(limits = c(40, 80), breaks = seq(40, 80, 10)) +
  my_theme

# ---------------------------
# 5. 组合图表
# ---------------------------
# 主可视化图
main_plot <- ggplot(table_data, aes(x = condition_label, y = percentage, fill = preference_label)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6, color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "white", fontface = "bold") +
  coord_flip() +
  labs(
    title = "共享情绪类型对群体偏好的影响",
    subtitle = "Alves等人(2018)研究复现",
    x = "实验条件",
    y = "比例",
    fill = "选择偏好"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent_format()) +
  my_theme +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "#7f8c8d", hjust = 0.5)
  )

# ---------------------------
# 6. 保存图表
# ---------------------------
# 保存主图表
ggsave("visualization_main.png", main_plot, width = 10, height = 7, dpi = 300, bg = "white")
cat("主可视化图已保存到: visualization_main.png\n")

# 保存组合图表
combined_plot <- grid.arrange(plot1, plot2, plot4, plot3, 
                              ncol = 2, 
                              top = textGrob("Alves et al. (2018) 研究复现可视化", 
                                             gp = gpar(fontsize = 16, fontface = "bold")))
ggsave("visualization_combined.png", combined_plot, width = 14, height = 10, dpi = 300, bg = "white")
cat("组合可视化图已保存到: visualization_combined.png\n")

# 保存统计结果图
ggsave("visualization_stats.png", plot3, width = 8, height = 4, dpi = 300, bg = "white")
cat("统计结果图已保存到: visualization_stats.png\n")

cat("\n✅ 所有可视化图片已生成完成！\n")