##@@ CLEANING FLOW TABLE: REPORT VERSION @@##

library(dplyr)

output_dir <- "C:/Users/Administrator/Desktop/论文复现/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

## -------------------------------
## Immediate condition
## -------------------------------

data.immediate_raw <- read.csv(
  "C:/Users/Administrator/Desktop/论文复现/rawdata_immediate.txt",
  sep = "\t",
  na.strings = "NaN"
)

immediate_question_cols <- c(
  paste0("q", 1:8, "a"),
  paste0("q", 1:8, "b")
)

data.immediate_raw$missing_responses <- rowSums(
  is.na(data.immediate_raw[, immediate_question_cols])
)

immediate_raw_N <- nrow(data.immediate_raw)

immediate_after_missing <- data.immediate_raw %>%
  filter(missing_responses == 0)

immediate_after_cheated <- immediate_after_missing %>%
  filter(cheated == "Nee")

immediate_flow <- data.frame(
  condition = c(
    "Immediate",
    "Immediate",
    "Immediate"
  ),
  step = c(
    "Raw data",
    "Exclude missing responses",
    "Exclude self-reported cheating"
  ),
  before_N = c(
    immediate_raw_N,
    immediate_raw_N,
    nrow(immediate_after_missing)
  ),
  excluded_N = c(
    NA,
    immediate_raw_N - nrow(immediate_after_missing),
    nrow(immediate_after_missing) - nrow(immediate_after_cheated)
  ),
  after_N = c(
    immediate_raw_N,
    nrow(immediate_after_missing),
    nrow(immediate_after_cheated)
  )
)


## -------------------------------
## Delayed condition
## -------------------------------

data.delayed_session_1_raw <- read.csv(
  "C:/Users/Administrator/Desktop/论文复现/rawdata_delayed_session1.txt",
  sep = "\t",
  na.strings = "NaN"
)

data.delayed_session_2_raw <- read.csv(
  "C:/Users/Administrator/Desktop/论文复现/rawdata_delayed_session2.txt",
  sep = "\t",
  na.strings = "NaN"
)

delayed_s1_N <- nrow(data.delayed_session_1_raw)
delayed_s2_N <- nrow(data.delayed_session_2_raw)

data.delayed_merged_raw <- merge(
  data.delayed_session_1_raw,
  data.delayed_session_2_raw,
  by = "student_number_simple",
  suffixes = c(".s1", ".s2")
)

delayed_merged_N <- nrow(data.delayed_merged_raw)

defocus_cols <- grep("^defocus", names(data.delayed_merged_raw), value = TRUE)
q_cols <- grep("^q", names(data.delayed_merged_raw), value = TRUE)

## 注意：这里不用 na.rm = TRUE，是为了和原 RPP 代码逻辑一致。
## 如果 defocus 中有 NA，rowSums 会产生 NA；
## 后续 filter(defocus_total == 0) 会排除这些 NA 行。
data.delayed_merged_raw$defocus_total <- rowSums(
  data.delayed_merged_raw[, defocus_cols]
)

data.delayed_merged_raw$missing_responses <- rowSums(
  is.na(data.delayed_merged_raw[, q_cols])
)

delayed_after_defocus <- data.delayed_merged_raw %>%
  filter(defocus_total == 0)

delayed_after_missing <- delayed_after_defocus %>%
  filter(missing_responses == 0)

delayed_after_peaking <- delayed_after_missing %>%
  filter(peaking == "Nee")

delayed_flow <- data.frame(
  condition = c(
    "Delayed",
    "Delayed",
    "Delayed",
    "Delayed",
    "Delayed",
    "Delayed"
  ),
  step = c(
    "Session 1 raw data",
    "Session 2 raw data",
    "Merge Session 1 and Session 2",
    "Exclude defocus_total not equal to 0, including NA",
    "Exclude missing responses",
    "Exclude self-reported peaking"
  ),
  before_N = c(
    NA,
    NA,
    NA,
    delayed_merged_N,
    nrow(delayed_after_defocus),
    nrow(delayed_after_missing)
  ),
  excluded_N = c(
    NA,
    NA,
    NA,
    delayed_merged_N - nrow(delayed_after_defocus),
    nrow(delayed_after_defocus) - nrow(delayed_after_missing),
    nrow(delayed_after_missing) - nrow(delayed_after_peaking)
  ),
  after_N = c(
    delayed_s1_N,
    delayed_s2_N,
    delayed_merged_N,
    nrow(delayed_after_defocus),
    nrow(delayed_after_missing),
    nrow(delayed_after_peaking)
  )
)

sample_cleaning_flow_report <- rbind(
  immediate_flow,
  delayed_flow
)

print(sample_cleaning_flow_report)

write.csv(
  sample_cleaning_flow_report,
  file = file.path(output_dir, "sample_cleaning_flow_report.csv"),
  row.names = FALSE
)
##@@ CREATE ANALYSIS DATASETS AND COMPUTE MSE @@##

## Correct answers from Steegen et al. (2014), Table 3
answers <- c(6.3, 43.3, 32.3, 13.4, 53.6, 54.8, 26.4, 22.4)

## -------------------------------
## Immediate condition: final analysis data
## -------------------------------

data.immediate <- immediate_after_cheated

## Guess 1 columns and Guess 2 columns
immediate_g1_cols <- paste0("q", 1:8, "a")
immediate_g2_cols <- paste0("q", 1:8, "b")

## Compute participant-level MSEs
data.immediate$mse.g1 <- rowMeans(
  sweep(data.immediate[, immediate_g1_cols], 2, answers, "-")^2
)

data.immediate$mse.g2 <- rowMeans(
  sweep(data.immediate[, immediate_g2_cols], 2, answers, "-")^2
)

data.immediate$mse.mean <- rowMeans(
  (
    (as.matrix(data.immediate[, immediate_g1_cols]) +
       as.matrix(data.immediate[, immediate_g2_cols])) / 2 -
      matrix(answers, nrow = nrow(data.immediate), ncol = 8, byrow = TRUE)
  )^2
)

## -------------------------------
## Delayed condition: final analysis data
## -------------------------------

data.delayed <- delayed_after_peaking

## Delayed columns after merge should usually be q1.s1 ... q8.s1 and q1.s2 ... q8.s2
## This part tries to detect them automatically.

delayed_s1_cols <- paste0("q", 1:8, ".s1")
delayed_s2_cols <- paste0("q", 1:8, ".s2")

if (!all(delayed_s1_cols %in% names(data.delayed))) {
  delayed_s1_cols <- paste0("q", 1:8, "a.s1")
}

if (!all(delayed_s2_cols %in% names(data.delayed))) {
  delayed_s2_cols <- paste0("q", 1:8, "a.s2")
}

if (!all(delayed_s1_cols %in% names(data.delayed))) {
  stop("Could not find delayed Session 1 question columns. Please check column names.")
}

if (!all(delayed_s2_cols %in% names(data.delayed))) {
  stop("Could not find delayed Session 2 question columns. Please check column names.")
}

data.delayed$mse.s1 <- rowMeans(
  sweep(data.delayed[, delayed_s1_cols], 2, answers, "-")^2
)

data.delayed$mse.s2 <- rowMeans(
  sweep(data.delayed[, delayed_s2_cols], 2, answers, "-")^2
)

data.delayed$mse.mean <- rowMeans(
  (
    (as.matrix(data.delayed[, delayed_s1_cols]) +
       as.matrix(data.delayed[, delayed_s2_cols])) / 2 -
      matrix(answers, nrow = nrow(data.delayed), ncol = 8, byrow = TRUE)
  )^2
)

## Check sample sizes and key means
cat("Immediate N:", nrow(data.immediate), "\n")
cat("Delayed N:", nrow(data.delayed), "\n")

cat("Immediate MSE means:\n")
print(colMeans(data.immediate[, c("mse.g1", "mse.g2", "mse.mean")]))

cat("Delayed MSE means:\n")
print(colMeans(data.delayed[, c("mse.s1", "mse.s2", "mse.mean")]))