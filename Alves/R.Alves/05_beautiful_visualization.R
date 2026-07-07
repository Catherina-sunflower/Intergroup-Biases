# ==============================================================================
# Alves, Koch & Unkelbach (2018) - 超美观可视化脚本
# ==============================================================================
# 创建专业级、出版质量的图表
# ==============================================================================

# 清空环境
rm(list = ls())
gc()

# 设置工作目录
setwd("D:/R语言大作业/Alves")

# 加载必要的包
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})

# 设置全局主题
theme_set(theme_minimal(base_size = 16, base_family = "Arial") +
            theme(
              plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 10)),
              plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
              axis.title = element_text(face = "bold", size = 14),
              axis.text = element_text(size = 12, color = "gray20"),
              legend.position = "bottom",
              legend.title = element_text(face = "bold", size = 12),
              legend.text = element_text(size = 11),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "gray90", size = 0.3),
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA)
            ))

# 定义配色方案（Nature期刊风格）
colors_nature <- c(
  "Condition 0" = "#3B4992",   # 深蓝
  "Condition 1" = "#DB0027",   # 深红
  "2nd Group" = "#8491B4B2",   # 浅蓝灰
  "1st Group" = "#E64B35B2",   # 珊瑚红
  "Negative" = "#4DBBD5",      # 青色
  "Positive" = "#E64B35",      # 红色
  "Indirect" = "#00A087",      # 绿色
  "Direct" = "#3C5488"         # 深紫蓝
)

# 创建输出目录
if (!dir.exists("figures/beautiful")) dir.create("figures/beautiful", recursive = TRUE)

# ==============================================================================
# 读取数据
# ==============================================================================

exp1_data <- read.table("2018_Alves_data/Exp1.dat", header = TRUE, sep = "\t")
exp2_data <- read.table("2018_Alves_data/Exp2.dat", header = TRUE, sep = "\t")
exp3_data <- read.table("2018_Alves_data/Exp3.dat", header = TRUE, sep = "\t")

# ==============================================================================
# 图1：实验1 - 精美的分组柱状图
# ==============================================================================

cat("创建实验1可视化...\n")

# 准备数据
exp1_plot_data <- exp1_data %>%
  mutate(
    condition_label = factor(condition,
                            levels = c(0, 1),
                            labels = c("Condition 0", "Condition 1")),
    preference_label = factor(preference_first,
                             levels = c(0, 1),
                             labels = c("2nd Group", "1st Group"))
  )

# 计算统计数据
exp1_stats <- exp1_plot_data %>%
  group_by(condition_label, preference_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(condition_label) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100,
    label_y = cumsum(count) - count / 2
  )

# 创建精美的柱状图
plot1 <- ggplot(exp1_stats, aes(x = condition_label, y = count, fill = preference_label)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75, alpha = 0.9) +
  geom_errorbar(aes(ymin = count, ymax = count),
                position = position_dodge(width = 0.75), width = 0.2, color = "gray30") +
  geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")),
            position = position_dodge(width = 0.75), vjust = -0.3, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = colors_nature[c("2nd Group", "1st Group")]) +
  labs(
    title = "Experiment 1: Group Preference by Condition",
    subtitle = "Alves, Koch & Unkelbach (2018) - Psychological Science",
    x = "Experimental Condition",
    y = "Number of Participants",
    fill = "Preference Choice"
  ) +
  ylim(0, 85) +
  annotate("text", x = 1, y = 80, label = "χ²(1) = 11.08, p < .001\nCramer's V = 0.23",
           size = 4, hjust = 0.5, fontface = "italic", color = "gray30") +
  guides(fill = guide_legend(reverse = TRUE))

# 保存
ggsave("figures/beautiful/exp1_beautiful_barplot.png", plot1,
       width = 12, height = 9, dpi = 300, bg = "white")
cat("✓ 实验1柱状图已保存\n")

# ==============================================================================
# 图2：实验1 - 比例对比图（带置信区间）
# ==============================================================================

# 计算比例和置信区间
exp1_prop <- exp1_plot_data %>%
  group_by(condition_label) %>%
  summarise(
    n = n(),
    prop_1st = mean(preference_first == 1),
    se = sqrt(prop_1st * (1 - prop_1st) / n),
    ci_lower = prop_1st - 1.96 * se,
    ci_upper = prop_1st + 1.96 * se,
    .groups = "drop"
  )

plot2 <- ggplot(exp1_prop, aes(x = condition_label, y = prop_1st, fill = condition_label)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, size = 1.2, color = "gray20") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "#E64B35", size = 1) +
  geom_text(aes(label = paste0(round(prop_1st * 100, 1), "%")),
            vjust = -2.5, size = 6, fontface = "bold", color = "gray20") +
  scale_fill_manual(values = colors_nature[c("Condition 0", "Condition 1")]) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.85), breaks = seq(0, 0.8, 0.2)) +
  labs(
    title = "Experiment 1: Proportion Preferring 1st Group",
    subtitle = "Error bars represent 95% confidence intervals; dashed line indicates chance level (50%)",
    x = "Experimental Condition",
    y = "Proportion Preferring 1st Group"
  ) +
  annotate("text", x = 1.5, y = 0.78, label = "Chance Level (50%)",
           color = "#E64B35", fontface = "italic", size = 4) +
  theme(legend.position = "none")

ggsave("figures/beautiful/exp1_proportion_plot.png", plot2,
       width = 11, height = 9, dpi = 300, bg = "white")
cat("✓ 实验1比例图已保存\n")

# ==============================================================================
# 图3：实验2 - Phi系数分布（小提琴图+箱线图）
# ==============================================================================

cat("\n创建实验2可视化...\n")

exp2_plot_data <- exp2_data %>%
  mutate(
    condition_label = factor(condition,
                            levels = c(0, 1),
                            labels = c("Condition 0", "Condition 1"))
  )

plot3 <- ggplot(exp2_plot_data, aes(x = condition_label, y = phi, fill = condition_label)) +
  geom_violin(alpha = 0.6, trim = FALSE, scale = "width", width = 0.7) +
  geom_boxplot(width = 0.25, alpha = 0.9, outlier.shape = NA, color = "gray20", size = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 2, color = "gray40") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, color = "white", fill = "yellow") +
  scale_fill_manual(values = colors_nature[c("Condition 0", "Condition 1")]) +
  labs(
    title = "Experiment 2: Phi Coefficient Distribution by Condition",
    subtitle = "Violin plots with boxplots and individual data points; yellow diamonds indicate means",
    x = "Experimental Condition",
    y = "Phi Coefficient",
    fill = "Condition"
  ) +
  annotate("text", x = 1.5, y = 0.95, label = "t(221) = -31.80, p < .001",
           size = 4.5, fontface = "italic", color = "gray30") +
  theme(legend.position = "none")

ggsave("figures/beautiful/exp2_phi_distribution.png", plot3,
       width = 11, height = 9, dpi = 300, bg = "white")
cat("✓ 实验2 Phi分布图已保存\n")

# ==============================================================================
# 图4：实验2 - 中介效应路径图
# ==============================================================================

# 创建中介效应数据
mediation_data <- data.frame(
  x = c(1, 2, 3),
  y = c(3, 2, 3),
  label = c("Condition", "Phi\n(Mediator)", "Preference"),
  node_type = c("IV", "Mediator", "DV")
)

# 路径数据
paths <- data.frame(
  x = c(1, 2, 1),
  xend = c(2, 3, 3),
  y = c(3, 2, 3),
  yend = c(2, 3, 3),
  label = c("a = 1.14***", "b = 0.38***", "c' = -0.25"),
  path_type = c("Indirect", "Indirect", "Direct")
)

plot4 <- ggplot() +
  # 绘制路径
  geom_segment(data = paths,
               aes(x = x, y = y, xend = xend, yend = yend, color = path_type),
               arrow = arrow(type = "closed", length = unit(0.3, "cm")),
               size = 2, alpha = 0.7) +
  # 添加路径标签
  geom_text(data = paths,
            aes(x = (x + xend) / 2, y = (y + yend) / 2 + 0.2, label = label, color = path_type),
            size = 5, fontface = "bold", vjust = -0.5) +
  # 绘制节点
  geom_point(data = mediation_data, aes(x = x, y = y, fill = node_type),
             shape = 21, size = 20, stroke = 2, color = "gray20") +
  # 添加节点标签
  geom_text(data = mediation_data, aes(x = x, y = y, label = label),
            size = 5, fontface = "bold", color = "white") +
  # 添加效应标签
  annotate("text", x = 2, y = 1.2, label = "Indirect Effect = 0.44***",
           size = 5, fontface = "bold", color = colors_nature["Indirect"]) +
  annotate("text", x = 2, y = 0.8, label = "Total Effect = 0.19**",
           size = 5, fontface = "bold", color = "gray30") +
  scale_fill_manual(values = c("IV" = "#3B4992", "Mediator" = "#00A087", "DV" = "#DB0027")) +
  scale_color_manual(values = c("Indirect" = "#00A087", "Direct" = "#3C5488")) +
  labs(
    title = "Experiment 2: Mediation Model",
    subtitle = "Condition → Phi → Preference (Standardized coefficients)",
    caption = "*** p < .001, ** p < .01"
  ) +
  xlim(0.5, 3.5) +
  ylim(0.5, 3.5) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    plot.caption = element_text(hjust = 0.5, size = 12, face = "italic"),
    legend.position = "none"
  )

ggsave("figures/beautiful/exp2_mediation_model.png", plot4,
       width = 10, height = 8, dpi = 300, bg = "white")
cat("✓ 实验2中介模型图已保存\n")

# ==============================================================================
# 图5：实验3 - Phi系数分布
# ==============================================================================

cat("\n创建实验3可视化...\n")

exp3_plot_data <- exp3_data %>%
  mutate(
    condition_label = factor(condition,
                            levels = c(0, 1),
                            labels = c("Condition 0", "Condition 1"))
  )

plot5 <- ggplot(exp3_plot_data, aes(x = condition_label, y = phi, fill = condition_label)) +
  geom_violin(alpha = 0.6, trim = FALSE, scale = "width", width = 0.7) +
  geom_boxplot(width = 0.25, alpha = 0.9, outlier.shape = NA, color = "gray20", size = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 2, color = "gray40") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, color = "white", fill = "yellow") +
  scale_fill_manual(values = colors_nature[c("Condition 0", "Condition 1")]) +
  labs(
    title = "Experiment 3: Phi Coefficient Distribution by Condition",
    subtitle = "Violin plots with boxplots and individual data points; yellow diamonds indicate means",
    x = "Experimental Condition",
    y = "Phi Coefficient",
    fill = "Condition"
  ) +
  annotate("text", x = 1.5, y = 0.95, label = "t(206) = -19.73, p < .001",
           size = 4.5, fontface = "italic", color = "gray30") +
  theme(legend.position = "none")

ggsave("figures/beautiful/exp3_phi_distribution.png", plot5,
       width = 11, height = 9, dpi = 300, bg = "white")
cat("✓ 实验3 Phi分布图已保存\n")

# ==============================================================================
# 图6：实验3 - 中介效应路径图
# ==============================================================================

paths3 <- data.frame(
  x = c(1, 2, 1),
  xend = c(2, 3, 3),
  y = c(3, 2, 3),
  yend = c(2, 3, 3),
  label = c("a = 0.88***", "b = 0.27***", "c' = -0.11"),
  path_type = c("Indirect", "Indirect", "Direct")
)

plot6 <- ggplot() +
  geom_segment(data = paths3,
               aes(x = x, y = y, xend = xend, yend = yend, color = path_type),
               arrow = arrow(type = "closed", length = unit(0.3, "cm")),
               size = 2, alpha = 0.7) +
  geom_text(data = paths3,
            aes(x = (x + xend) / 2, y = (y + yend) / 2 + 0.2, label = label, color = path_type),
            size = 5, fontface = "bold", vjust = -0.5) +
  geom_point(data = mediation_data, aes(x = x, y = y, fill = node_type),
             shape = 21, size = 20, stroke = 2, color = "gray20") +
  geom_text(data = mediation_data, aes(x = x, y = y, label = label),
            size = 5, fontface = "bold", color = "white") +
  annotate("text", x = 2, y = 1.2, label = "Indirect Effect = 0.23***",
           size = 5, fontface = "bold", color = colors_nature["Indirect"]) +
  annotate("text", x = 2, y = 0.8, label = "Total Effect = 0.13",
           size = 5, fontface = "bold", color = "gray30") +
  scale_fill_manual(values = c("IV" = "#3B4992", "Mediator" = "#00A087", "DV" = "#DB0027")) +
  scale_color_manual(values = c("Indirect" = "#00A087", "Direct" = "#3C5488")) +
  labs(
    title = "Experiment 3: Mediation Model",
    subtitle = "Condition → Phi → Preference (Standardized coefficients)",
    caption = "*** p < .001"
  ) +
  xlim(0.5, 3.5) +
  ylim(0.5, 3.5) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    plot.caption = element_text(hjust = 0.5, size = 12, face = "italic"),
    legend.position = "none"
  )

ggsave("figures/beautiful/exp3_mediation_model.png", plot6,
       width = 10, height = 8, dpi = 300, bg = "white")
cat("✓ 实验3中介模型图已保存\n")

# ==============================================================================
# 图7：三个实验对比图
# ==============================================================================

cat("\n创建三个实验对比图...\n")

# 准备对比数据
comparison_data <- data.frame(
  experiment = rep(c("Exp 1", "Exp 2", "Exp 3"), each = 2),
  condition = rep(c("Condition 0", "Condition 1"), 3),
  prop_1st = c(
    mean(exp1_data$preference_first[exp1_data$condition == 0]),
    mean(exp1_data$preference_first[exp1_data$condition == 1]),
    mean(exp2_data$preference_first[exp2_data$condition == 0]),
    mean(exp2_data$preference_first[exp2_data$condition == 1]),
    mean(exp3_data$preference_first[exp3_data$condition == 0]),
    mean(exp3_data$preference_first[exp3_data$condition == 1])
  )
)

plot7 <- ggplot(comparison_data, aes(x = experiment, y = prop_1st, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "#E64B35", size = 1) +
  geom_text(aes(label = paste0(round(prop_1st * 100, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = colors_nature[c("Condition 0", "Condition 1")]) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.85)) +
  labs(
    title = "Comparison Across All Three Experiments",
    subtitle = "Proportion preferring 1st group by condition; dashed line indicates chance level (50%)",
    x = "Experiment",
    y = "Proportion Preferring 1st Group",
    fill = "Condition"
  ) +
  theme(legend.position = "bottom")

ggsave("figures/beautiful/all_experiments_comparison.png", plot7,
       width = 12, height = 9, dpi = 300, bg = "white")
cat("✓ 三个实验对比图已保存\n")

# ==============================================================================
# 完成
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("所有可视化图表创建完成！\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("生成的图表：\n")
cat("1. figures/beautiful/exp1_beautiful_barplot.png - 实验1精美柱状图\n")
cat("2. figures/beautiful/exp1_proportion_plot.png - 实验1比例图\n")
cat("3. figures/beautiful/exp2_phi_distribution.png - 实验2 Phi分布图\n")
cat("4. figures/beautiful/exp2_mediation_model.png - 实验2中介模型图\n")
cat("5. figures/beautiful/exp3_phi_distribution.png - 实验3 Phi分布图\n")
cat("6. figures/beautiful/exp3_mediation_model.png - 实验3中介模型图\n")
cat("7. figures/beautiful/all_experiments_comparison.png - 三个实验对比图\n")
