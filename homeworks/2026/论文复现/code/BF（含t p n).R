##@@ FULL BAYES + REPRODUCED t/n/p @@##

library(BayesFactor)
library(dplyr)

output_dir <- "C:/Users/Administrator/Desktop/论文复现/output"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

## Helper functions
get_paired_t <- function(x, y) {
  test <- t.test(x, y, paired = TRUE)
  return(c(t = test$statistic, df = test$parameter, p = test$p.value, n = length(x)))
}

get_independent_t <- function(x, y) {
  test <- t.test(x, y, paired = FALSE)
  return(c(t = test$statistic, df = test$parameter, p = test$p.value, n1 = length(x), n2 = length(y)))
}

## 计算复现 BF10 / BF01
get_paired_bf01 <- function(x, y, label) {
  bf10 <- BayesFactor::ttestBF(x = x, y = y, paired = TRUE, rscale = "medium") %>%
    BayesFactor::extractBF() %>% .$bf
  bf01 <- 1 / bf10
  data.frame(
    analysis = label,
    test_type = "paired t-test",
    BF10 = bf10,
    BF01 = bf01
  )
}

get_independent_bf01 <- function(x, y, label) {
  bf10 <- BayesFactor::ttestBF(x = x, y = y, paired = FALSE, rscale = "medium") %>%
    BayesFactor::extractBF() %>% .$bf
  bf01 <- 1 / bf10
  data.frame(
    analysis = label,
    test_type = "independent t-test",
    BF10 = bf10,
    BF01 = bf01
  )
}

## Averaging benefit
data.immediate$benefit_g1 <- data.immediate$mse.g1 - data.immediate$mse.mean
data.immediate$benefit_g2 <- data.immediate$mse.g2 - data.immediate$mse.mean
data.delayed$benefit_g1 <- data.delayed$mse.s1 - data.delayed$mse.mean
data.delayed$benefit_g2 <- data.delayed$mse.s2 - data.delayed$mse.mean

## -------------------------------------------------------------------------
## 1. 复现 t / n / p
## -------------------------------------------------------------------------
repro_tn <- data.frame(
  analysis = c(
    "Immediate: Guess 1 vs Average",
    "Immediate: Guess 2 vs Average",
    "Delayed: Guess 1 vs Average",
    "Delayed: Guess 2 vs Average",
    "Immediate: Guess 1 vs Guess 2",
    "Delayed: Guess 1 vs Guess 2",
    "Averaging benefit based on Guess 1: Delayed vs Immediate",
    "Averaging benefit based on Guess 2: Delayed vs Immediate"
  ),
  stringsAsFactors = FALSE
)

# paired t-tests
repro_tn[1, c("t","df","p","n")] <- get_paired_t(data.immediate$mse.g1, data.immediate$mse.mean)
repro_tn[2, c("t","df","p","n")] <- get_paired_t(data.immediate$mse.g2, data.immediate$mse.mean)
repro_tn[3, c("t","df","p","n")] <- get_paired_t(data.delayed$mse.s1, data.delayed$mse.mean)
repro_tn[4, c("t","df","p","n")] <- get_paired_t(data.delayed$mse.s2, data.delayed$mse.mean)
repro_tn[5, c("t","df","p","n")] <- get_paired_t(data.immediate$mse.g1, data.immediate$mse.g2)
repro_tn[6, c("t","df","p","n")] <- get_paired_t(data.delayed$mse.s1, data.delayed$mse.s2)

# independent t-tests for averaging benefit
tmp <- get_independent_t(data.delayed$benefit_g1, data.immediate$benefit_g1)
repro_tn[7, c("t","df","p","n1","n2")] <- tmp
tmp <- get_independent_t(data.delayed$benefit_g2, data.immediate$benefit_g2)
repro_tn[8, c("t","df","p","n1","n2")] <- tmp

## -------------------------------------------------------------------------
## 2. 复现 Bayes factor
## -------------------------------------------------------------------------
bf_results <- rbind(
  get_paired_bf01(data.immediate$mse.g1, data.immediate$mse.mean, "Immediate: Guess 1 vs Average"),
  get_paired_bf01(data.immediate$mse.g2, data.immediate$mse.mean, "Immediate: Guess 2 vs Average"),
  get_paired_bf01(data.delayed$mse.s1, data.delayed$mse.mean, "Delayed: Guess 1 vs Average"),
  get_paired_bf01(data.delayed$mse.s2, data.delayed$mse.mean, "Delayed: Guess 2 vs Average"),
  get_paired_bf01(data.immediate$mse.g1, data.immediate$mse.g2, "Immediate: Guess 1 vs Guess 2"),
  get_paired_bf01(data.delayed$mse.s1, data.delayed$mse.s2, "Delayed: Guess 1 vs Guess 2"),
  get_independent_bf01(data.delayed$benefit_g1, data.immediate$benefit_g1, "Averaging benefit based on Guess 1: Delayed vs Immediate"),
  get_independent_bf01(data.delayed$benefit_g2, data.immediate$benefit_g2, "Averaging benefit based on Guess 2: Delayed vs Immediate")
)

## -------------------------------------------------------------------------
## 3. 合并 t/n/p 和 BF
## -------------------------------------------------------------------------
bayes_repro_full <- merge(repro_tn, bf_results, by = "analysis")

## 4. 增加解释和 δ 计算
bayes_repro_full$BF01_interpretation <- ifelse(
  bayes_repro_full$BF01 < 1,
  "Evidence favors H1 / difference",
  "Evidence favors H0 / no difference"
)

## 保存文件
write.csv(bayes_repro_full, file = file.path(output_dir,"bayes_table5_repro_full.csv"), row.names = FALSE)