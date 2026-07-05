##@@ TABLE 7: COMPUTATIONAL REPRODUCIBILITY PE RATING @@##

output_dir <- "C:/Users/Administrator/Desktop/论文复现/output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

## 原文报告值来自 Steegen et al. (2014) Table 4 及正文中的 dz 95% CI。
## 复现值来自本次 R 分析输出。
## p 值由于原文多报告为 p < .001，不作为 PE 计算对象。

pe_data <- data.frame(
  condition = c(
    rep("Immediate", 20),
    rep("Delayed", 20)
  ),
  comparison = c(
    rep("Guess 1 vs Average", 10),
    rep("Guess 2 vs Average", 10),
    rep("Guess 1 vs Average", 10),
    rep("Guess 2 vs Average", 10)
  ),
  statistic = rep(
    c(
      "Single_MSE_M",
      "Single_MSE_SD",
      "Average_MSE_M",
      "Average_MSE_SD",
      "r",
      "n",
      "t",
      "dz",
      "dz_CI_lower",
      "dz_CI_upper"
    ),
    4
  ),
  original_value = c(
    ## Immediate: Guess 1 vs Average
    589, 336, 541, 313, 0.93, 471, 8.69, 0.40, 0.31, 0.49,
    ## Immediate: Guess 2 vs Average
    615, 351, 541, 313, 0.90, 471, 10.26, 0.47, 0.38, 0.57,
    ## Delayed: Guess 1 vs Average
    517, 288, 467, 260, 0.86, 140, 4.02, 0.34, 0.17, 0.51,
    ## Delayed: Guess 2 vs Average
    589, 327, 467, 260, 0.86, 140, 8.48, 0.72, 0.53, 0.90
  ),
  reproduced_value = c(
    ## Immediate: Guess 1 vs Average
    589.2927, 336.1419, 541.4940, 313.4072, 0.9347874, 471, 8.687830, 0.4003141, 0.3062518, 0.4939807,
    ## Immediate: Guess 2 vs Average
    614.9216, 350.5750, 541.4940, 313.4072, 0.8965589, 471, 10.262460, 0.4728692, 0.3774088, 0.5678751,
    ## Delayed: Guess 1 vs Average
    517.4279, 288.0740, 467.4558, 260.2884, 0.8610119, 140, 4.021702, 0.3398959, 0.1689276, 0.5097064,
    ## Delayed: Guess 2 vs Average
    588.8852, 326.7183, 467.4558, 260.2884, 0.8571413, 140, 8.479422, 0.7166420, 0.5298029, 0.9014085
  )
)

## 计算百分误差 δ / PE
pe_data$delta_percent <- abs(pe_data$original_value - pe_data$reproduced_value) /
  abs(pe_data$original_value) * 100

## 按课程指南评级
pe_data$rating <- ifelse(
  pe_data$delta_percent == 0,
  "完全一致 δ = 0%",
  ifelse(
    pe_data$delta_percent < 10,
    "偏差较小 0% < δ < 10%",
    "偏差较大 δ >= 10%"
  )
)

## 标记可能原因
pe_data$note <- ifelse(
  pe_data$delta_percent == 0,
  "完全一致",
  "主要由原文四舍五入导致"
)

## 保存详细表
write.csv(
  pe_data,
  file = file.path(output_dir, "reproducibility_pe_detailed.csv"),
  row.names = FALSE
)

## 汇总表
pe_summary <- as.data.frame(table(pe_data$rating))
names(pe_summary) <- c("reproducibility_rating", "N")
pe_summary$percent <- round(pe_summary$N / sum(pe_summary$N) * 100, 1)

## 补充课程模板中的项目
all_levels <- data.frame(
  reproducibility_rating = c(
    "完全一致 δ = 0%",
    "偏差较小 0% < δ < 10%",
    "偏差较大 δ >= 10%",
    "因舍入导致的偏差",
    "无法进行可重复检验"
  )
)

pe_summary_full <- merge(
  all_levels,
  pe_summary,
  by = "reproducibility_rating",
  all.x = TRUE
)

pe_summary_full$N[is.na(pe_summary_full$N)] <- 0
pe_summary_full$percent[is.na(pe_summary_full$percent)] <- 0

## 因舍入导致的偏差 = 所有非零但小于 10% 的偏差
rounding_N <- sum(pe_data$delta_percent > 0 & pe_data$delta_percent < 10)
rounding_percent <- round(rounding_N / nrow(pe_data) * 100, 1)

pe_summary_full$N[
  pe_summary_full$reproducibility_rating == "因舍入导致的偏差"
] <- rounding_N

pe_summary_full$percent[
  pe_summary_full$reproducibility_rating == "因舍入导致的偏差"
] <- rounding_percent

## 按模板顺序排序
pe_summary_full <- pe_summary_full[match(
  all_levels$reproducibility_rating,
  pe_summary_full$reproducibility_rating
), ]

## 增加总计
pe_summary_full <- rbind(
  pe_summary_full,
  data.frame(
    reproducibility_rating = "总计",
    N = nrow(pe_data),
    percent = 100
  )
)

write.csv(
  pe_summary_full,
  file = file.path(output_dir, "reproducibility_pe_summary.csv"),
  row.names = FALSE
)

print(pe_data)
print(pe_summary_full)

cat("Table 7 PE rating files saved to:", output_dir, "\n")