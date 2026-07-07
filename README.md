# 计算可复现性检验课程作业

本文件夹包含**心理学院 R 编程语言课程**的计算可复现性检验作业，对 Alves, Koch & Unkelbach (2018) 发表在 *Psychological Science* 上的研究进行了两套独立的复现分析。

---

## 目录结构

```
D:\R语言大作业/
├── Alves/                       # 复现项目一：使用 OSF 原始数据 (.dat) 的完整复现
│   ├── 2018_Alves_data/         # OSF 下载的原始数据
│   ├── R.Alves/                 # R 分析脚本
│   ├── figures/                 # 可视化输出
│   │   ├── beautiful/           #   精美可视化图表（柱状图、中介模型路径图等）
│   │   └── figures_comparison/  # 复现对比图（6 面板综合图 + 8 张分面板）
│   ├── Supplemental Materials/  # 原文补充材料（实验刺激图片 alien1.jpg, alien2.jpg）
│   └── *.md                     # 复现报告及说明文档
│
├── Replication/                  # 复现项目二：Gleibs 等研究者采集的独立数据复现
│   ├── Replication_data/        # 原始数据及清洗后数据
│   ├── R.Replication/           # R 分析脚本
│   ├── figures/                 # 可视化输出
│   └── *.csv / *.pdf            # 数据字典及文献
│
└── README.md                    # 本文档
```

---

## 项目一：Alves/ — OSF 原始数据复现

基于 OSF 下载的原始数据文件（`Exp1.dat`、`Exp2.dat`、`Exp3.dat`），使用与原文献相同的统计方法（Pearson 卡方检验、phi 系数分组分析、lavaan 稳健中介效应模型）进行完整复现。

### 数据

| 文件 | 说明 |
|------|------|
| `2018_Alves_data/Exp1.dat` | 实验 1 原始数据（condition, preference_first, N=210） |
| `2018_Alves_data/Exp2.dat` | 实验 2 原始数据（condition, phi, preference_first, N=223） |
| `2018_Alves_data/Exp3.dat` | 实验 3 原始数据（condition, phi, preference_first, N=208） |
| `2018_Alves_data/variables.txt` | 变量说明文档 |
| `2018_Alves_data/orig_data.xlsx` | 原始 Excel 数据 |
| `2018_Alves_data/finedbyexcel_data.xlsx` | Excel 整理后数据 |

### 脚本

| 文件 | 说明 |
|------|------|
| `R.Alves/fined_data.R` | 核心分析脚本：卡方检验、phi 分组、中介效应（lavaan）、结果汇总 |
| `R.Alves/visualization_comparison.R` | 对比可视化脚本：生成 6 面板综合复现对比图 |
| `R.Alves/origindata_code.R` | 原始分析代码 |
| `R.Alves/05_beautiful_visualization.R` | 精美可视化脚本（柱状图、中介模型路径图等） |

### 可视化

| 路径 | 说明 |
|------|------|
| `figures/figures_comparison/reproducibility_comparison_comprehensive.png` | 6 面板综合复现对比图 |
| `figures/figures_comparison/A_chi_square.png` ~ `H_consistency.png` | 8 张分面板对比图 |
| `figures/beautiful/exp1_beautiful_barplot.png` | 实验 1 精美柱状图 |
| `figures/beautiful/exp2_mediation_model.png` | 实验 2 中介模型路径图 |
| `figures/beautiful/exp3_mediation_model.png` | 实验 3 中介模型路径图 |
| `figures/beautiful/all_experiments_comparison.png` | 三实验综合对比图 |

### 文档

| 文件 | 说明 |
|------|------|
| `README.md` | Alves 项目说明（旧版，含详细实验设计介绍） |
| `对AlvesKochUnkelbach(2018)研究结果的可复现性检验报告.md` | **完整复现报告**（含引言、方法、结果、讨论、参考文献） |
| `复现思路说明.md` | 复现方法与思路的详细说明 |
| `卡方检验方法修正说明.md` | Yates 校正与 Pearson 卡方的差异说明及修正验证 |
| `复现结果汇总.csv` | 三个实验的卡方值、p 值、phi 系数、phi 分组统计量 |
| `项目完成总结.md` | 项目完成情况总结 |
| `AlvesKochUnkelbach2018.pdf` | 原文 PDF |
| `Supplemental Materials/` | 原文补充材料（实验刺激图片） |

---

## 项目二：Replication/ — Gleibs 独立数据复现

由 Ilka H. Gleibs 和 Nihan Albayrak-Aydemir（LSE）收集的独立复现数据，采用 2×2 被试间设计（N=361），使用虚构外星人部落作为实验材料。

### 数据

| 文件 | 说明 |
|------|------|
| `Replication_data/Alves_PsychologSci_2018_AvOr-Gleibs_92g_raw.csv` | 原始调查数据（361 名参与者，来自 Qualtrics） |
| `Replication_data/Alves_PsychologSci_2018_AvOr-Gleibs_92g_Analysis_cleaned.csv` | 清洗后分析数据 |
| `Replication_data/Alves_PsychologSci_2018_AvOr-Gleibs_92g_Analysis.csv` | 分析数据 |
| `Replication_data/Replication_cleaned_data.csv` | 清洗后数据 |
| `DataDictionary_Alves_PsySci_2018_AvOr_Gleibs.csv` | 数据字典（含变量名、测量单位、允许值、说明） |

### 脚本

| 文件 | 说明 |
|------|------|
| `R.Replication/Replication_code.R` | 主分析脚本：数据导入、清洗、排除标准、卡方检验 |
| `R.Replication/Further_analysis.R` | 进一步分析：排除未通过注意力检查的参与者、检验性别和年龄的影响 |
| `R.Replication/Further_analysis_optimized.R` | 优化版进一步分析 |
| `R.Replication/Replication2.R` | 辅助分析脚本 |
| `R.Replication/visualization_script.R` | 可视化脚本 |
| `R.Replication/create_chinese_plots.R` | 中文标注图表生成脚本 |
| `R.Replication/复现思路_新版.md` | 复现思路说明文档 |

### 可视化

| 文件 | 说明 |
|------|------|
| `figures/Alves_Replication_BarPlot.png` | 复现结果柱状图 |
| `figures/condition_preference_plot.png` | 条件 × 偏好分布图 |
| `figures/Figure1_Before_Exclusion.png` | 排除前结果图 |
| `figures/Figure2_After_Exclusion.png` | 排除后结果图 |

### 其他文件

| 文件 | 说明 |
|------|------|
| `复现-Alves_Psycholog_Sci_2018_AvOr_Gleibs_92 copy.pdf` | 复现报告 PDF |
| `.venv/` | Python 虚拟环境 |

---

## 关键发现对比

| 项目 | 数据来源 | N | 分析方法 | 复现结果 |
|------|---------|---|---------|---------|
| **Alves/** | OSF 原始数据 (.dat) | 210 / 223 / 208 | Pearson χ², phi 分组, lavaan 中介 | 卡方值、phi 系数 100% 匹配原文；74.3% 指标 PE=0% |
| **Replication/** | Gleibs 独立收集 (Qualtrics) | 361 | 卡方检验、排除分析 | 独立验证原始假设 |

---

## 技术说明

- **分析环境：** R 4.x
- **主要 R 包：** tidyverse, lavaan, vcd, ggplot2, gridExtra, scales
- **中介效应模型：** `lavaan::sem(se="robust")` 稳健标准误
- **卡方检验：** `chisq.test(correct=FALSE)` Pearson 卡方（无 Yates 校正）

---

*最后更新：2026 年 7 月*
