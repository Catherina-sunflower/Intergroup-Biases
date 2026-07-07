# ==============================================================================
# Alves et al. (2018) - 中文版美观可视化
# ==============================================================================

setwd("D:/R语言大作业/Replication")

library(ggplot2)
library(dplyr)

# ---------------------------
# 1. 数据导入与预处理
# ---------------------------
raw_data <- read.csv("Replication_data/Alves_PsychologSci_2018_AvOr-Gleibs_92g_raw.csv", 
                     stringsAsFactors = FALSE, encoding = "UTF-8")

cols_to_remove <- c("StartDate", "EndDate", "Status", "IPAddress",
                    "RecordedDate", "DistributionChannel", "UserLanguage",
                    "PROLIFIC_PID", "Finished","gender_3_TEXT","ethnicity_10_TEXT","comments")
data <- raw_data[, !names(raw_data) %in% cols_to_remove]

data <- data[-1, ]
data <- subset(data, tribe_preference != "" & !is.na(tribe_preference))

dat <- data %>% select(condition, tribe_preference, attention_check)
dat$condition <- as.numeric(dat$condition)
dat$tribe_preference <- as.numeric(dat$tribe_preference)

# ---------------------------
# 2. 变量重编码
# ---------------------------
dat$condition_rec <- ifelse(dat$condition %in% c(1, 3), "SP", "SN")

dat$tribe_preference_rec <- NA
mask_12 <- dat$condition %in% c(1, 2)
dat$tribe_preference_rec[mask_12] <- dat$tribe_preference[mask_12]
mask_34 <- dat$condition %in% c(3, 4)
dat$tribe_preference_rec[mask_34] <- 1 - dat$tribe_preference[mask_34]

# ---------------------------
# 3. 创建排除前后的数据
# ---------------------------
exclude_answers <- c("tulip", "Favorite", "dump", "plut")
dat_after <- dat[!dat$attention_check %in% exclude_answers, ]

# ---------------------------
# 4. 创建美观的可视化
# ---------------------------

# 使用优雅的配色方案
# 配色方案1: 深海蓝-珊瑚红
# color_palette <- c("#1E3A5F", "#E85D75")
# 配色方案2: 翡翠绿-薰衣草紫
# color_palette <- c("#20BF6B", "#A29BFE")
# 配色方案3: 日落橙-海洋蓝
color_palette <- c("#F39C12", "#3498DB")
# 配色方案4: 玫瑰红-薄荷绿
# color_palette <- c("#E91E63", "#00E676")

create_plot <- function(data, title, subtitle_text) {
  table_data <- data %>%
    group_by(condition_rec, tribe_preference_rec) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(
      condition_label = ifelse(condition_rec == "SP", "共享正面情绪", "共享负面情绪"),
      preference_label = ifelse(tribe_preference_rec == 0, "第一个部落", "第二个部落")
    )
  
  ggplot(table_data, aes(x = condition_label, y = count, fill = preference_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, 
             color = "white", linewidth = 0.8,
             show.legend = TRUE) +
    geom_text(aes(label = paste0(count, " (", round(count/sum(count)*100, 1), "%)")), 
              position = position_dodge(width = 0.7), 
              vjust = -0.5, size = 3.5, color = "#2C3E50", fontface = "bold") +
    labs(
      title = title,
      subtitle = subtitle_text,
      x = "实验条件",
      y = "人数",
      fill = "偏好选择"
    ) +
    scale_fill_manual(values = color_palette, 
                      labels = c("选择第一个呈现的部落", "选择第二个呈现的部落")) +
    scale_y_continuous(limits = c(0, 130), breaks = seq(0, 130, 20)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "#2C3E50", hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 15)),
      axis.title = element_text(size = 12, color = "#34495E", margin = margin(t = 10)),
      axis.text = element_text(size = 11, color = "#34495E"),
      axis.line = element_line(color = "#ECF0F1"),
      axis.ticks = element_line(color = "#ECF0F1"),
      legend.position = "bottom",
      legend.title = element_text(size = 11, color = "#2C3E50"),
      legend.text = element_text(size = 10, color = "#34495E"),
      legend.spacing.x = unit(0.5, "cm"),
      panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white")
    )
}

# ---------------------------
# 5. 生成可视化图表
# ---------------------------

# 排除前的图
plot_before <- create_plot(dat, "偏好分布（排除前）", 
                          paste0("总人数 = ", nrow(dat), "（所有参与者）"))
ggsave("Figure1_Before_Exclusion.png", plot_before, width = 10, height = 7, dpi = 300, bg = "white")
cat("Figure1_Before_Exclusion.png 已保存\n")

# 排除后的图
plot_after <- create_plot(dat_after, "偏好分布（排除后）", 
                         paste0("总人数 = ", nrow(dat_after), "（排除5名未通过注意力检查者）"))
ggsave("Figure2_After_Exclusion.png", plot_after, width = 10, height = 7, dpi = 300, bg = "white")
cat("Figure2_After_Exclusion.png 已保存\n")

cat("\n✅ 中文版可视化图已生成完成！\n")