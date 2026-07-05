##@@ SAMPLE AND CLEANING FLOW TABLE @@##

# Make sure output directory exists
output_dir <- "C:/Users/Administrator/Desktop/论文复现/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

## -------------------------------
## Immediate condition cleaning
## -------------------------------

# If data.immediate_raw does not exist, reload raw immediate data
data.immediate_raw <- read.csv(
  "C:/Users/Administrator/Desktop/论文复现/rawdata_immediate.txt",
  sep = "\t",
  na.strings = "NaN"
)

immediate_raw_N <- nrow(data.immediate_raw)

# Missing responses: q1a-q8a and q1b-q8b
data.immediate_raw$missing_responses <- data.immediate_raw %>%
  dplyr::select(q1a:q8a, q1b:q8b) %>%
  is.na() %>%
  rowSums()

immediate_missing_N <- sum(data.immediate_raw$missing_responses > 0, na.rm = TRUE)

# Cheating exclusion
immediate_cheated_N <- sum(data.immediate_raw$cheated != "Nee", na.rm = TRUE)

# Sequential cleaning
immediate_after_missing <- data.immediate_raw %>%
  dplyr::filter(missing_responses == 0)

immediate_excluded_cheated_after_missing <- sum(
  immediate_after_missing$cheated != "Nee",
  na.rm = TRUE
)

immediate_clean <- immediate_after_missing %>%
  dplyr::filter(cheated == "Nee")

immediate_final_N <- nrow(immediate_clean)


## -------------------------------
## Delayed condition cleaning
## -------------------------------

# If raw delayed data objects do not exist, reload them
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

delayed_s1_raw_N <- nrow(data.delayed_session_1_raw)
delayed_s2_raw_N <- nrow(data.delayed_session_2_raw)

# Merge two delayed sessions
data.delayed_merged_raw <- merge(
  data.delayed_session_1_raw,
  data.delayed_session_2_raw,
  by = "student_number_simple",
  suffixes = c(".s1", ".s2")
)

delayed_merged_N <- nrow(data.delayed_merged_raw)

# Compute defocus and missing
data.delayed_merged_raw$defocus_total <- data.delayed_merged_raw %>%
  dplyr::select(starts_with("defocus")) %>%
  rowSums()

data.delayed_merged_raw$missing_responses <- data.delayed_merged_raw %>%
  dplyr::select(starts_with("q")) %>%
  is.na() %>%
  rowSums()

delayed_defocus_N <- sum(data.delayed_merged_raw$defocus_total != 0, na.rm = TRUE)
delayed_missing_N <- sum(data.delayed_merged_raw$missing_responses > 0, na.rm = TRUE)
delayed_peaking_N <- sum(data.delayed_merged_raw$peaking != "Nee", na.rm = TRUE)

# Sequential cleaning
delayed_after_defocus <- data.delayed_merged_raw %>%
  dplyr::filter(defocus_total == 0)

delayed_excluded_missing_after_defocus <- sum(
  delayed_after_defocus$missing_responses > 0,
  na.rm = TRUE
)

delayed_after_missing <- delayed_after_defocus %>%
  dplyr::filter(missing_responses == 0)

delayed_excluded_peaking_after_previous <- sum(
  delayed_after_missing$peaking != "Nee",
  na.rm = TRUE
)

delayed_clean <- delayed_after_missing %>%
  dplyr::filter(peaking == "Nee")

delayed_final_N <- nrow(delayed_clean)


## -------------------------------
## Build cleaning table
## -------------------------------

sample_cleaning_table <- data.frame(
  condition = c(
    "Immediate",
    "Immediate",
    "Immediate",
    "Immediate",
    "Delayed",
    "Delayed",
    "Delayed",
    "Delayed",
    "Delayed",
    "Delayed"
  ),
  step = c(
    "Raw data",
    "Excluded: missing responses",
    "Excluded: cheated after missing-response exclusion",
    "Final analytic sample",
    "Session 1 raw data",
    "Session 2 raw data",
    "Merged two sessions",
    "Excluded: defocus",
    "Excluded: missing responses after defocus exclusion",
    "Excluded: peaking after previous exclusions"
  ),
  N = c(
    immediate_raw_N,
    immediate_missing_N,
    immediate_excluded_cheated_after_missing,
    immediate_final_N,
    delayed_s1_raw_N,
    delayed_s2_raw_N,
    delayed_merged_N,
    delayed_defocus_N,
    delayed_excluded_missing_after_defocus,
    delayed_excluded_peaking_after_previous
  )
)

# Add final delayed row
sample_cleaning_table <- rbind(
  sample_cleaning_table,
  data.frame(
    condition = "Delayed",
    step = "Final analytic sample",
    N = delayed_final_N
  )
)

print(sample_cleaning_table)

write.csv(
  sample_cleaning_table,
  file = file.path(output_dir, "sample_cleaning_table.csv"),
  row.names = FALSE
)

cat("Sample cleaning table saved to:",
    file.path(output_dir, "sample_cleaning_table.csv"),
    "\n")