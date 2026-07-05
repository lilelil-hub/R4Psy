# Match-Moral 条件下 RT 的自我优势效应 (SPE) 分析
# 数据：match_raw.csv

library(data.table)
library(dplyr)

# 1. 读取数据
df <- fread("E:/R lesson/R4Psy/slides/data/match/match_raw.csv")

# 2. 筛选 Match-Moral 条件
# Match = "match" 且 Label 包含 "moral"
df_moral <- df[Match == "match" & grepl("moral", Label)]

# 3. RT 筛选：0.2 - 1.5 秒
df_filtered <- df_moral[RT >= 0.2 & RT <= 1.5]

# 4. 创建 Self/Other 分类
# Self: moralSelf
# Other: moralOther
df_filtered[, condition := ifelse(Label == "moralSelf", "Self", "Other")]

# 5. 按被试和条件计算平均 RT
df_summary <- df_filtered[, .(mean_RT = mean(RT)), by = .(Sub, condition)]

# 6. 转换为宽格式（Self 和 Other 并列）
df_wide <- dcast(df_summary, Sub ~ condition, value.var = "mean_RT")

# 7. 计算 SPE = Self - Other
df_spe <- df_wide %>%
  dplyr::mutate(
    moral_SPE = Self - Other
  ) %>%
  dplyr::select(Sub, moral_SPE)

# 8. 输出结果
cat("=== Match-Moral 条件下的 SPE ===\n\n")
print(df_spe)

cat("\n=== SPE 描述统计 ===\n")
summary(df_spe$moral_SPE)