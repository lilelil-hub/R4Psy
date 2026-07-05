##@@ DESCRIPTIVE STATISTICS: MATCH STEEGEN ET AL. (2014) @@##

library(dplyr)

output_dir <- "C:/Users/Administrator/Desktop/论文复现/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

## ---------------------------------------------------------
## 0. Check required objects
## ---------------------------------------------------------

if (!exists("data.immediate")) {
  stop("Object 'data.immediate' not found. Please run cleaning and MSE calculation first.")
}

if (!exists("data.delayed")) {
  stop("Object 'data.delayed' not found. Please run cleaning and MSE calculation first.")
}

required_immediate_mse <- c("mse.g1", "mse.g2", "mse.mean")
required_delayed_mse <- c("mse.s1", "mse.s2", "mse.mean")

if (!all(required_immediate_mse %in% names(data.immediate))) {
  stop("Immediate MSE variables not found. Need mse.g1, mse.g2, mse.mean.")
}

if (!all(required_delayed_mse %in% names(data.delayed))) {
  stop("Delayed MSE variables not found. Need mse.s1, mse.s2, mse.mean.")
}

## ---------------------------------------------------------
## 1. Helper functions
## ---------------------------------------------------------

mean_sd <- function(x) {
  c(
    M = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE)
  )
}

find_first_existing <- function(data, possible_names) {
  hit <- possible_names[possible_names %in% names(data)]
  if (length(hit) == 0) {
    return(NA)
  } else {
    return(hit[1])
  }
}

## ---------------------------------------------------------
## 2. Sample descriptive statistics
##    原文 3.1 Sample: N, gender counts, age M and SD
## ---------------------------------------------------------

## 可能的年龄列、性别列名称。
## 如果你的数据列名不同，可以在这里加。
possible_age_cols <- c(
  "age", "leeftijd", "Age", "AGE",
  "age.s1", "leeftijd.s1"
)

possible_gender_cols <- c(
  "sex", "gender", "geslacht", "Sex", "Gender",
  "sex.s1", "gender.s1", "geslacht.s1"
)

age_col_immediate <- find_first_existing(data.immediate, possible_age_cols)
gender_col_immediate <- find_first_existing(data.immediate, possible_gender_cols)

age_col_delayed <- find_first_existing(data.delayed, possible_age_cols)
gender_col_delayed <- find_first_existing(data.delayed, possible_gender_cols)

sample_descriptives <- data.frame(
  Condition = c("Immediate", "Delayed"),
  N = c(nrow(data.immediate), nrow(data.delayed)),
  Age_column_used = c(age_col_immediate, age_col_delayed),
  Gender_column_used = c(gender_col_immediate, gender_col_delayed),
  Age_M = NA_real_,
  Age_SD = NA_real_,
  stringsAsFactors = FALSE
)

if (!is.na(age_col_immediate)) {
  sample_descriptives$Age_M[1] <- mean(data.immediate[[age_col_immediate]], na.rm = TRUE)
  sample_descriptives$Age_SD[1] <- sd(data.immediate[[age_col_immediate]], na.rm = TRUE)
}

if (!is.na(age_col_delayed)) {
  sample_descriptives$Age_M[2] <- mean(data.delayed[[age_col_delayed]], na.rm = TRUE)
  sample_descriptives$Age_SD[2] <- sd(data.delayed[[age_col_delayed]], na.rm = TRUE)
}

## 性别人数表
gender_counts_immediate <- NULL
gender_counts_delayed <- NULL

if (!is.na(gender_col_immediate)) {
  gender_counts_immediate <- as.data.frame(table(data.immediate[[gender_col_immediate]]))
  names(gender_counts_immediate) <- c("Gender", "N")
  gender_counts_immediate$Condition <- "Immediate"
}

if (!is.na(gender_col_delayed)) {
  gender_counts_delayed <- as.data.frame(table(data.delayed[[gender_col_delayed]]))
  names(gender_counts_delayed) <- c("Gender", "N")
  gender_counts_delayed$Condition <- "Delayed"
}

gender_counts <- bind_rows(gender_counts_immediate, gender_counts_delayed) %>%
  select(Condition, Gender, N)

## ---------------------------------------------------------
## 3. MSE descriptive statistics matching Table 4
## ---------------------------------------------------------

mse_descriptives <- data.frame(
  Condition = c(
    "Immediate", "Immediate",
    "Delayed", "Delayed"
  ),
  Comparison = c(
    "Guess 1 vs Average",
    "Guess 2 vs Average",
    "Guess 1 vs Average",
    "Guess 2 vs Average"
  ),
  Single_MSE_M = c(
    mean(data.immediate$mse.g1, na.rm = TRUE),
    mean(data.immediate$mse.g2, na.rm = TRUE),
    mean(data.delayed$mse.s1, na.rm = TRUE),
    mean(data.delayed$mse.s2, na.rm = TRUE)
  ),
  Single_MSE_SD = c(
    sd(data.immediate$mse.g1, na.rm = TRUE),
    sd(data.immediate$mse.g2, na.rm = TRUE),
    sd(data.delayed$mse.s1, na.rm = TRUE),
    sd(data.delayed$mse.s2, na.rm = TRUE)
  ),
  Average_MSE_M = c(
    mean(data.immediate$mse.mean, na.rm = TRUE),
    mean(data.immediate$mse.mean, na.rm = TRUE),
    mean(data.delayed$mse.mean, na.rm = TRUE),
    mean(data.delayed$mse.mean, na.rm = TRUE)
  ),
  Average_MSE_SD = c(
    sd(data.immediate$mse.mean, na.rm = TRUE),
    sd(data.immediate$mse.mean, na.rm = TRUE),
    sd(data.delayed$mse.mean, na.rm = TRUE),
    sd(data.delayed$mse.mean, na.rm = TRUE)
  ),
  r = c(
    cor(data.immediate$mse.g1, data.immediate$mse.mean, use = "complete.obs"),
    cor(data.immediate$mse.g2, data.immediate$mse.mean, use = "complete.obs"),
    cor(data.delayed$mse.s1, data.delayed$mse.mean, use = "complete.obs"),
    cor(data.delayed$mse.s2, data.delayed$mse.mean, use = "complete.obs")
  ),
  n = c(
    nrow(data.immediate),
    nrow(data.immediate),
    nrow(data.delayed),
    nrow(data.delayed)
  ),
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------
## 4. Averaging benefit descriptive statistics
##    原文正文报告了 benefit 的 M 和 SD
## ---------------------------------------------------------

data.immediate$benefit_g1 <- data.immediate$mse.g1 - data.immediate$mse.mean
data.immediate$benefit_g2 <- data.immediate$mse.g2 - data.immediate$mse.mean

data.delayed$benefit_g1 <- data.delayed$mse.s1 - data.delayed$mse.mean
data.delayed$benefit_g2 <- data.delayed$mse.s2 - data.delayed$mse.mean

benefit_descriptives <- data.frame(
  Benefit = c(
    "Guess 1 MSE - Average MSE",
    "Guess 1 MSE - Average MSE",
    "Guess 2 MSE - Average MSE",
    "Guess 2 MSE - Average MSE"
  ),
  Condition = c(
    "Immediate",
    "Delayed",
    "Immediate",
    "Delayed"
  ),
  M = c(
    mean(data.immediate$benefit_g1, na.rm = TRUE),
    mean(data.delayed$benefit_g1, na.rm = TRUE),
    mean(data.immediate$benefit_g2, na.rm = TRUE),
    mean(data.delayed$benefit_g2, na.rm = TRUE)
  ),
  SD = c(
    sd(data.immediate$benefit_g1, na.rm = TRUE),
    sd(data.delayed$benefit_g1, na.rm = TRUE),
    sd(data.immediate$benefit_g2, na.rm = TRUE),
    sd(data.delayed$benefit_g2, na.rm = TRUE)
  ),
  N = c(
    nrow(data.immediate),
    nrow(data.delayed),
    nrow(data.immediate),
    nrow(data.delayed)
  ),
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------
## 5. Rounded report versions
## ---------------------------------------------------------

sample_descriptives_report <- sample_descriptives
sample_descriptives_report$Age_M <- round(sample_descriptives_report$Age_M, 2)
sample_descriptives_report$Age_SD <- round(sample_descriptives_report$Age_SD, 2)

mse_descriptives_report <- mse_descriptives
mse_descriptives_report$Single_MSE_M <- round(mse_descriptives_report$Single_MSE_M, 2)
mse_descriptives_report$Single_MSE_SD <- round(mse_descriptives_report$Single_MSE_SD, 2)
mse_descriptives_report$Average_MSE_M <- round(mse_descriptives_report$Average_MSE_M, 2)
mse_descriptives_report$Average_MSE_SD <- round(mse_descriptives_report$Average_MSE_SD, 2)
mse_descriptives_report$r <- round(mse_descriptives_report$r, 3)

benefit_descriptives_report <- benefit_descriptives
benefit_descriptives_report$M <- round(benefit_descriptives_report$M, 2)
benefit_descriptives_report$SD <- round(benefit_descriptives_report$SD, 2)

## ---------------------------------------------------------
## 6. Print outputs
## ---------------------------------------------------------

cat("\nSample descriptives:\n")
print(sample_descriptives_report)

cat("\nGender counts:\n")
print(gender_counts)

cat("\nMSE descriptives matching Table 4:\n")
print(mse_descriptives_report)

cat("\nAveraging benefit descriptives:\n")
print(benefit_descriptives_report)

## ---------------------------------------------------------
## 7. Save outputs
## ---------------------------------------------------------

write.csv(
  sample_descriptives_report,
  file = file.path(output_dir, "descriptive_sample_statistics.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  gender_counts,
  file = file.path(output_dir, "descriptive_gender_counts.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  mse_descriptives_report,
  file = file.path(output_dir, "descriptive_mse_table4.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  benefit_descriptives_report,
  file = file.path(output_dir, "descriptive_averaging_benefit.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

## Optional: save all in one Excel workbook
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

library(openxlsx)

wb <- createWorkbook()

addWorksheet(wb, "Sample")
writeData(wb, "Sample", sample_descriptives_report)

addWorksheet(wb, "Gender")
writeData(wb, "Gender", gender_counts)

addWorksheet(wb, "MSE_Table4")
writeData(wb, "MSE_Table4", mse_descriptives_report)

addWorksheet(wb, "Benefit")
writeData(wb, "Benefit", benefit_descriptives_report)

saveWorkbook(
  wb,
  file = file.path(output_dir, "descriptive_statistics_all.xlsx"),
  overwrite = TRUE
)

cat(
  "\nDescriptive statistics files saved to:",
  output_dir,
  "\n"
)