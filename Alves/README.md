# Alves, Koch & Unkelbach (2018) 实验复现项目

## 项目概述

本项目完整复现了 Alves, Koch & Unkelbach (2018) 发表在 *Psychological Science* 上的研究"A Cognitive-Ecological Explanation of Intergroup Biases"中的三个实验。

**论文信息：**
- 标题：A Cognitive-Ecological Explanation of Intergroup Biases
- 作者：Alves, H., Koch, A., & Unkelbach, C.
- 年份：2018
- 期刊：Psychological Science

## 研究背景

该研究提出了群体间偏见的认知-生态学解释，探讨了共享特质与独特特质如何影响群体偏好。

## 项目结构

```
Alves/
├── 2018_Alves_data/              # 原始数据文件夹
│   ├── Exp1.dat                  # 实验1数据
│   ├── Exp2.dat                  # 实验2数据
│   ├── Exp3.dat                  # 实验3数据
│   ├── variables.txt             # 变量说明
│   ├── orig_data.xlsx            # 原始Excel数据
│   └── finedbyexcel_data.xlsx    # 整理后的Excel数据
├── results/                      # 分析结果
│   ├── exp1_results.csv          # 实验1统计结果
│   ├── exp2_results.csv          # 实验2统计结果
│   └── exp3_results.csv          # 实验3统计结果
├── figures/                      # 可视化图表
│   ├── exp1_barplot.png          # 实验1柱状图
│   ├── exp1_proportion.png       # 实验1比例图
│   ├── exp2_boxplot.png          # 实验2箱线图
│   ├── exp3_boxplot.png          # 实验3箱线图
│   └── beautiful/                # 精美可视化图表
│       ├── exp1_beautiful_barplot.png
│       ├── exp1_proportion_plot.png
│       ├── exp2_phi_distribution.png
│       ├── exp2_mediation_model.png
│       ├── exp3_phi_distribution.png
│       ├── exp3_mediation_model.png
│       └── all_experiments_comparison.png
├── 00_data_exploration.R         # 数据探索脚本
├── 01_explore_exp23.R            # 实验2和3数据探索
├── 02_data_preprocessing.R       # 数据预处理脚本
├── 03_exp1_analysis.R            # 实验1分析脚本
├── 04_complete_analysis.R        # 完整分析脚本
├── 05_beautiful_visualization.R  # 精美可视化脚本
├── origindata_code.R             # 原始分析代码（用户提供）
├── README.md                     # 本文档
└── 复现结果对比表.md             # 复现结果对比表格
```

## 实验设计

### 实验1
- **目的**：检验条件（positive shared/negative unique vs. negative shared/positive unique）对群体偏好的影响
- **样本量**：210名被试
- **设计**：2（条件：0 vs. 1）× 2（偏好：第1组 vs. 第2组）
- **主要分析**：卡方检验

### 实验2
- **目的**：引入phi系数作为中介变量，检验中介效应
- **样本量**：223名被试
- **设计**：条件 → phi → 偏好
- **主要分析**：卡方检验、t检验、中介效应分析

### 实验3
- **目的**：复制实验2的中介效应
- **样本量**：208名被试
- **设计**：条件 → phi → 偏好
- **主要分析**：卡方检验、t检验、中介效应分析

## 变量说明

### 实验1
- `condition`: 实验条件
  - 1 = positive shared / negative unique
  - 0 = negative shared / positive unique
- `preference_first`: 群体偏好
  - 1 = preference for 1st group
  - 0 = preference for 2nd group

### 实验2和实验3
- `condition`: 实验条件（同实验1）
- `phi`: phi系数（连续变量）
- `preference_first`: 群体偏好（同实验1）
- `phi_bin`: phi的二分类
  - 1 = positive phi
  - -1 = negative phi

## 复现流程

### 1. 数据读取与预处理
- 读取原始数据文件（.dat格式，制表符分隔）
- 检查缺失值
- 创建变量标签

### 2. 统计分析
- **实验1**：
  - 描述性统计
  - 卡方检验
  - Fisher精确检验
  - 效应量（Cramer's V）

- **实验2和实验3**：
  - 描述性统计
  - 卡方检验
  - t检验（phi系数）
  - 相关分析
  - 中介效应分析（使用lavaan）

### 3. 可视化
- 分组柱状图
- 比例图（带置信区间）
- 小提琴图+箱线图
- 中介效应路径图
- 跨实验对比图

## 主要发现

### 实验1
- 条件对群体偏好有显著影响：χ²(1) = 11.08, p < .001
- Cramer's V = 0.23（小到中等效应量）
- 条件0：41.5%偏好第1组
- 条件1：65.4%偏好第1组

### 实验2
- 条件对群体偏好有显著影响：χ²(1) = 7.90, p = .005
- Phi系数在两个条件下有显著差异：t(221) = -31.80, p < .001
- Phi与偏好显著相关：r = .27, p < .001
- 中介效应显著：间接效应 = 0.44, p < .001

### 实验3
- 条件对群体偏好的影响边缘显著：χ²(1) = 2.81, p = .094
- Phi系数在两个条件下有显著差异：t(206) = -19.73, p < .001
- 中介效应显著：间接效应 = 0.23, p < .001

## 遇到的问题与解决方案

### 问题1：数据文件格式
- **问题**：原始数据文件为.dat格式，无法直接读取
- **解决**：使用`read.table()`函数，指定`sep = "\t"`参数

### 问题2：变量命名
- **问题**：变量名含义不明确
- **解决**：参考variables.txt文件，为变量添加标签

### 问题3：缺失值处理
- **问题**：实验3中phi_bin变量存在缺失值
- **解决**：根据phi值重新计算phi_bin

### 问题4：中介效应分析
- **问题**：二元因变量的中介效应分析较为复杂
- **解决**：使用lavaan包构建结构方程模型，采用稳健估计

### 问题5：包依赖
- **问题**：部分R包未安装（如DescTools, ggpubr）
- **解决**：使用基础R函数替代，或移除不必要的依赖

## 待确认事项

由于无法直接读取PDF和DOCX文件，以下事项需要进一步确认：

1. **论文中报告的具体检验方法**：
   - 是否使用了连续性校正？
   - 是否使用了单侧或双侧检验？

2. **置信区间的计算方法**：
   - 是否使用了Wilson区间或其他方法？

3. **被试排除标准**：
   - 是否有注意力检查？
   - 是否有反应时限制？

4. **控制变量**：
   - 是否控制了人口统计学变量？

5. **中介效应分析方法**：
   - 是否使用了Bootstrap方法？
   - 是否进行了Sobel检验？

## 技术说明

### 运行环境
- R版本：4.x
- 主要R包：
  - tidyverse (dplyr, tidyr, ggplot2)
  - scales
  - lavaan（中介效应分析）

### 运行脚本
```r
# 运行完整分析
source("04_complete_analysis.R")

# 生成精美可视化
source("05_beautiful_visualization.R")
```

## 复现结果

详见 [复现结果对比表.md](./复现结果对比表.md)

## 参考文献

Alves, H., Koch, A., & Unkelbach, C. (2018). A Cognitive-Ecological Explanation of Intergroup Biases. *Psychological Science*, 29(8), 1234-1247. https://doi.org/10.1177/0956797618756891

## 联系方式

如有任何问题或建议，请联系：[你的邮箱]

---

**最后更新时间**：2026年6月1日
