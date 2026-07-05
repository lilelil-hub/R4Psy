##@@ TABLE 4 SUPPLEMENTARY PARAMETERS @@##
## This section computes:
## 1. Correlation r between single-guess MSE and mean-estimate MSE
## 2. 95% CI for Cohen's dz using the noncentral t distribution
## 3. Detectability approach / d33% threshold

## -------------------------------------------------------------------------
## Function 1: Cohen's dz and 95% CI from paired t-test
## dz = t / sqrt(n)
## CI is obtained from the noncentral t confidence interval.
## -------------------------------------------------------------------------

ci_dz_from_t <- function(t_value, n, conf.level = 0.95) {
  df <- n - 1
  alpha <- 1 - conf.level
  
  # Handle positive and negative t values
  sign_t <- sign(t_value)
  t_abs <- abs(t_value)
  
  # Lower and upper noncentrality parameter bounds
  # For positive t:
  # lower ncp solves CDF(t_obs; ncp) = 1 - alpha/2
  # upper ncp solves CDF(t_obs; ncp) = alpha/2
  
  lower_ncp <- uniroot(
    function(ncp) {
      pt(t_abs, df = df, ncp = ncp) - (1 - alpha / 2)
    },
    lower = -1000,
    upper = t_abs
  )$root
  
  upper_ncp <- uniroot(
    function(ncp) {
      pt(t_abs, df = df, ncp = ncp) - (alpha / 2)
    },
    lower = t_abs,
    upper = 1000
  )$root
  
  ci_pos <- c(lower_ncp, upper_ncp) / sqrt(n)
  
  # If original t is negative, flip interval direction
  if (sign_t < 0) {
    ci <- -rev(ci_pos)
  } else {
    ci <- ci_pos
  }
  
  names(ci) <- c("ci_lower", "ci_upper")
  return(ci)
}


## -------------------------------------------------------------------------
## Function 2: d33% threshold
## d33 is the effect size that the original study had 33% power to detect.
## For paired designs, this is equivalent to a paired t-test power calculation.
## -------------------------------------------------------------------------

compute_d33 <- function(n_original, power_target = 0.33, alpha = 0.05) {
  root <- uniroot(
    function(d) {
      power.t.test(
        n = n_original,
        delta = d,
        sd = 1,
        sig.level = alpha,
        power = NULL,
        type = "paired",
        alternative = "two.sided"
      )$power - power_target
    },
    lower = 0.0001,
    upper = 2
  )$root
  
  return(root)
}


## -------------------------------------------------------------------------
## Original Vul & Pashler (2008) sample sizes
## Immediate condition: df = 254, so n = 255
## Delayed condition: df = 172, so n = 173
## -------------------------------------------------------------------------

n_original_immediate <- 255
n_original_delayed <- 173

d33_immediate <- compute_d33(n_original_immediate)
d33_delayed <- compute_d33(n_original_delayed)

d33_immediate
d33_delayed


## -------------------------------------------------------------------------
## Helper function: summarize one comparison
## -------------------------------------------------------------------------

summarize_table4_comparison <- function(single_mse, mean_mse, condition, comparison, d33_value) {
  
  test_one_tailed <- t.test(
    single_mse,
    mean_mse,
    alternative = "greater",
    paired = TRUE
  )
  
  n <- length(single_mse)
  t_value <- as.numeric(test_one_tailed$statistic)
  dz <- t_value / sqrt(n)
  dz_ci <- ci_dz_from_t(t_value, n)
  
  r_value <- cor(single_mse, mean_mse, use = "complete.obs")
  
  out <- data.frame(
    condition = condition,
    comparison = comparison,
    N = n,
    
    single_mse_mean = mean(single_mse, na.rm = TRUE),
    single_mse_sd = sd(single_mse, na.rm = TRUE),
    mean_mse_mean = mean(mean_mse, na.rm = TRUE),
    mean_mse_sd = sd(mean_mse, na.rm = TRUE),
    
    r_single_mean = r_value,
    
    t_value = t_value,
    df = as.numeric(test_one_tailed$parameter),
    p_value_one_tailed = test_one_tailed$p.value,
    
    cohen_dz = dz,
    dz_ci_lower = dz_ci["ci_lower"],
    dz_ci_upper = dz_ci["ci_upper"],
    
    d33_threshold = d33_value,
    dz_greater_than_d33 = dz > d33_value,
    ci_lower_greater_than_d33 = dz_ci["ci_lower"] > d33_value,
    
    stringsAsFactors = FALSE
  )
  
  return(out)
}


## -------------------------------------------------------------------------
## Compute Table 4 supplementary results for four core comparisons
## -------------------------------------------------------------------------

table4_supplementary <- rbind(
  summarize_table4_comparison(
    single_mse = data.immediate$mse.g1,
    mean_mse = data.immediate$mse.mean,
    condition = "Immediate",
    comparison = "Guess 1 vs Average",
    d33_value = d33_immediate
  ),
  
  summarize_table4_comparison(
    single_mse = data.immediate$mse.g2,
    mean_mse = data.immediate$mse.mean,
    condition = "Immediate",
    comparison = "Guess 2 vs Average",
    d33_value = d33_immediate
  ),
  
  summarize_table4_comparison(
    single_mse = data.delayed$mse.s1,
    mean_mse = data.delayed$mse.mean,
    condition = "Delayed",
    comparison = "Guess 1 vs Average",
    d33_value = d33_delayed
  ),
  
  summarize_table4_comparison(
    single_mse = data.delayed$mse.s2,
    mean_mse = data.delayed$mse.mean,
    condition = "Delayed",
    comparison = "Guess 2 vs Average",
    d33_value = d33_delayed
  )
)

print(table4_supplementary)


## -------------------------------------------------------------------------
## Rounded version for report
## -------------------------------------------------------------------------

table4_supplementary_rounded <- table4_supplementary

numeric_cols <- sapply(table4_supplementary_rounded, is.numeric)
table4_supplementary_rounded[numeric_cols] <- lapply(
  table4_supplementary_rounded[numeric_cols],
  function(x) round(x, 3)
)

print(table4_supplementary_rounded)


## -------------------------------------------------------------------------
## Save outputs
## -------------------------------------------------------------------------
rownames(table4_supplementary) <- NULL
rownames(table4_supplementary_rounded) <- NULL
write.csv(
  table4_supplementary,
  file = file.path(output_dir, "table4_supplementary_parameters2.csv"),
  row.names = FALSE
)

write.csv(
  table4_supplementary_rounded,
  file = file.path(output_dir, "table4_supplementary_parameters_rounded2.csv"),
  row.names = FALSE
)

cat("Table 4 supplementary parameters saved to:",
    file.path(output_dir, "table4_supplementary_parameters2.csv"),
    "\n")