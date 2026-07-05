##@@ ADDITIONAL ANALYSIS: AVERAGING BENEFIT COMPARISON @@##
## Compare whether the averaging benefit is larger in the delayed condition
## than in the immediate condition.

## Averaging benefit = single-guess MSE - mean-estimate MSE
## Larger value means greater improvement from averaging two guesses.

# Benefit based on Guess 1 / Session 1
data.delayed$benefit_g1 <- data.delayed$mse.s1 - data.delayed$mse.mean
data.immediate$benefit_g1 <- data.immediate$mse.g1 - data.immediate$mse.mean

# Benefit based on Guess 2 / Session 2
data.delayed$benefit_g2 <- data.delayed$mse.s2 - data.delayed$mse.mean
data.immediate$benefit_g2 <- data.immediate$mse.g2 - data.immediate$mse.mean


##@@ TEST 1: Benefit based on Guess 1 @@##
## Original question:
## Is the averaging benefit based on the first guess larger in delayed than immediate?

test_benefit_g1 <- t.test(
  data.delayed$benefit_g1,
  data.immediate$benefit_g1,
  alternative = "two.sided",
  var.equal = TRUE
)

print(test_benefit_g1)

# Cohen's d for independent-samples t-test
n_delayed <- nrow(data.delayed)
n_immediate <- nrow(data.immediate)

d_benefit_g1 <- as.numeric(test_benefit_g1$statistic) *
  sqrt(1 / n_delayed + 1 / n_immediate)

d_benefit_g1


##@@ TEST 2: Benefit based on Guess 2 @@##
## Post-hoc question:
## Is the averaging benefit based on the second guess larger in delayed than immediate?

test_benefit_g2 <- t.test(
  data.delayed$benefit_g2,
  data.immediate$benefit_g2,
  alternative = "two.sided",
  var.equal = TRUE
)

print(test_benefit_g2)

# Cohen's d for independent-samples t-test
d_benefit_g2 <- as.numeric(test_benefit_g2$statistic) *
  sqrt(1 / n_delayed + 1 / n_immediate)

d_benefit_g2


##@@ DESCRIPTIVE STATISTICS FOR BENEFITS @@##

benefit_descriptive <- data.frame(
  condition = c(
    "Delayed",
    "Immediate",
    "Delayed",
    "Immediate"
  ),
  benefit_type = c(
    "Guess1_MSE_minus_Mean_MSE",
    "Guess1_MSE_minus_Mean_MSE",
    "Guess2_MSE_minus_Mean_MSE",
    "Guess2_MSE_minus_Mean_MSE"
  ),
  N = c(
    n_delayed,
    n_immediate,
    n_delayed,
    n_immediate
  ),
  mean_benefit = c(
    mean(data.delayed$benefit_g1, na.rm = TRUE),
    mean(data.immediate$benefit_g1, na.rm = TRUE),
    mean(data.delayed$benefit_g2, na.rm = TRUE),
    mean(data.immediate$benefit_g2, na.rm = TRUE)
  ),
  sd_benefit = c(
    sd(data.delayed$benefit_g1, na.rm = TRUE),
    sd(data.immediate$benefit_g1, na.rm = TRUE),
    sd(data.delayed$benefit_g2, na.rm = TRUE),
    sd(data.immediate$benefit_g2, na.rm = TRUE)
  )
)

print(benefit_descriptive)


##@@ SAVE RESULTS @@##

benefit_inferential <- data.frame(
  analysis = c(
    "Averaging benefit based on Guess 1: delayed vs immediate",
    "Averaging benefit based on Guess 2: delayed vs immediate"
  ),
  delayed_N = c(n_delayed, n_delayed),
  immediate_N = c(n_immediate, n_immediate),
  delayed_mean_benefit = c(
    mean(data.delayed$benefit_g1, na.rm = TRUE),
    mean(data.delayed$benefit_g2, na.rm = TRUE)
  ),
  immediate_mean_benefit = c(
    mean(data.immediate$benefit_g1, na.rm = TRUE),
    mean(data.immediate$benefit_g2, na.rm = TRUE)
  ),
  t_value = c(
    as.numeric(test_benefit_g1$statistic),
    as.numeric(test_benefit_g2$statistic)
  ),
  df = c(
    as.numeric(test_benefit_g1$parameter),
    as.numeric(test_benefit_g2$parameter)
  ),
  p_value = c(
    test_benefit_g1$p.value,
    test_benefit_g2$p.value
  ),
  cohen_d = c(
    d_benefit_g1,
    d_benefit_g2
  ),
  alternative = c(
    "two.sided",
    "two.sided"
  ),
  var_equal = c(
    TRUE,
    TRUE
  )
)

print(benefit_inferential)

write.csv(
  benefit_descriptive,
  file = file.path(output_dir, "averaging_benefit_descriptive.csv"),
  row.names = FALSE
)

write.csv(
  benefit_inferential,
  file = file.path(output_dir, "averaging_benefit_comparison_results.csv"),
  row.names = FALSE
)

cat("Averaging benefit comparison results saved to:",
    file.path(output_dir, "averaging_benefit_comparison_results.csv"),
    "\n")