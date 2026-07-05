##@@ BAYES FACTOR ANALYSIS @@##
## This section computes JZS Bayes factors for the main and supplementary tests.
## The BayesFactor package returns BF10 by default:
## BF10 = evidence for alternative hypothesis over null hypothesis.
## Steegen et al. (2014) Table 5 reports Bayes factors in favor of the null hypothesis.
## Therefore, we report BF01 = 1 / BF10.

## Install package if needed
if (!requireNamespace("BayesFactor", quietly = TRUE)) {
  install.packages("BayesFactor")
}

library(BayesFactor)

## Helper function: paired-sample BF
get_paired_bf01 <- function(x, y, label) {
  bf10_obj <- BayesFactor::ttestBF(
    x = x,
    y = y,
    paired = TRUE,
    rscale = "medium"
  )
  
  bf10 <- BayesFactor::extractBF(bf10_obj)$bf
  bf01 <- 1 / bf10
  
  data.frame(
    analysis = label,
    test_type = "paired t-test",
    BF10 = bf10,
    BF01 = bf01,
    stringsAsFactors = FALSE
  )
}

## Helper function: independent-sample BF
get_independent_bf01 <- function(x, y, label) {
  bf10_obj <- BayesFactor::ttestBF(
    x = x,
    y = y,
    paired = FALSE,
    rscale = "medium"
  )
  
  bf10 <- BayesFactor::extractBF(bf10_obj)$bf
  bf01 <- 1 / bf10
  
  data.frame(
    analysis = label,
    test_type = "independent t-test",
    BF10 = bf10,
    BF01 = bf01,
    stringsAsFactors = FALSE
  )
}

## Make sure averaging benefit variables exist
data.immediate$benefit_g1 <- data.immediate$mse.g1 - data.immediate$mse.mean
data.immediate$benefit_g2 <- data.immediate$mse.g2 - data.immediate$mse.mean

data.delayed$benefit_g1 <- data.delayed$mse.s1 - data.delayed$mse.mean
data.delayed$benefit_g2 <- data.delayed$mse.s2 - data.delayed$mse.mean


## -------------------------------------------------------------------------
## Bayes factors corresponding to Steegen et al. (2014) Table 5
## -------------------------------------------------------------------------

bayes_results <- rbind(
  ## Confirmatory tests: average estimate vs single estimates
  
  get_paired_bf01(
    x = data.immediate$mse.g1,
    y = data.immediate$mse.mean,
    label = "Immediate: Guess 1 vs Average"
  ),
  
  get_paired_bf01(
    x = data.immediate$mse.g2,
    y = data.immediate$mse.mean,
    label = "Immediate: Guess 2 vs Average"
  ),
  
  get_paired_bf01(
    x = data.delayed$mse.s1,
    y = data.delayed$mse.mean,
    label = "Delayed: Guess 1 vs Average"
  ),
  
  get_paired_bf01(
    x = data.delayed$mse.s2,
    y = data.delayed$mse.mean,
    label = "Delayed: Guess 2 vs Average"
  ),
  
  ## Guess 1 vs Guess 2
  
  get_paired_bf01(
    x = data.immediate$mse.g1,
    y = data.immediate$mse.g2,
    label = "Immediate: Guess 1 vs Guess 2"
  ),
  
  get_paired_bf01(
    x = data.delayed$mse.s1,
    y = data.delayed$mse.s2,
    label = "Delayed: Guess 1 vs Guess 2"
  ),
  
  ## Averaging benefit comparison: delayed vs immediate
  
  get_independent_bf01(
    x = data.delayed$benefit_g1,
    y = data.immediate$benefit_g1,
    label = "Averaging benefit based on Guess 1: Delayed vs Immediate"
  ),
  
  get_independent_bf01(
    x = data.delayed$benefit_g2,
    y = data.immediate$benefit_g2,
    label = "Averaging benefit based on Guess 2: Delayed vs Immediate"
  )
)

## Add simple interpretation
bayes_results$interpretation <- ifelse(
  bayes_results$BF01 < 1,
  "Evidence favors H1 / difference",
  "Evidence favors H0 / no difference"
)

## Rounded version for report
bayes_results_rounded <- bayes_results
bayes_results_rounded$BF10 <- signif(bayes_results_rounded$BF10, 4)
bayes_results_rounded$BF01 <- signif(bayes_results_rounded$BF01, 4)

print(bayes_results_rounded)

## Save outputs
write.csv(
  bayes_results,
  file = file.path(output_dir, "bayes_factor_results.csv"),
  row.names = FALSE
)

write.csv(
  bayes_results_rounded,
  file = file.path(output_dir, "bayes_factor_results_rounded.csv"),
  row.names = FALSE
)

cat("Bayes factor results saved to:",
    file.path(output_dir, "bayes_factor_results.csv"),
    "\n")