##@@ ADDITIONAL ANALYSIS: GUESS 1 VS GUESS 2 @@##
## This analysis compares the MSE of the first guess and the second guess.
## It checks whether the second guess was less accurate than the first guess.

# Delayed condition: Session 1 vs Session 2
test_delayed_g1_vs_g2 <- t.test(
  data.delayed$mse.s1,
  data.delayed$mse.s2,
  alternative = "two.sided",
  paired = TRUE
)

print(test_delayed_g1_vs_g2)

# Effect size dz for delayed condition
n_delayed <- nrow(data.delayed)
d_z_delayed_g1_vs_g2 <- as.numeric(test_delayed_g1_vs_g2$statistic) / sqrt(n_delayed)

d_z_delayed_g1_vs_g2


# Immediate condition: Guess 1 vs Guess 2
test_immediate_g1_vs_g2 <- t.test(
  data.immediate$mse.g1,
  data.immediate$mse.g2,
  alternative = "two.sided",
  paired = TRUE
)

print(test_immediate_g1_vs_g2)

# Effect size dz for immediate condition
n_immediate <- nrow(data.immediate)
d_z_immediate_g1_vs_g2 <- as.numeric(test_immediate_g1_vs_g2$statistic) / sqrt(n_immediate)

d_z_immediate_g1_vs_g2


##@@ SAVE RESULTS @@##

guess1_vs_guess2_results <- data.frame(
  analysis = c(
    "Delayed condition: mse.s1 vs mse.s2",
    "Immediate condition: mse.g1 vs mse.g2"
  ),
  N = c(
    n_delayed,
    n_immediate
  ),
  mean_guess1 = c(
    mean(data.delayed$mse.s1, na.rm = TRUE),
    mean(data.immediate$mse.g1, na.rm = TRUE)
  ),
  mean_guess2 = c(
    mean(data.delayed$mse.s2, na.rm = TRUE),
    mean(data.immediate$mse.g2, na.rm = TRUE)
  ),
  mean_difference = c(
    mean(data.delayed$mse.s1 - data.delayed$mse.s2, na.rm = TRUE),
    mean(data.immediate$mse.g1 - data.immediate$mse.g2, na.rm = TRUE)
  ),
  t_value = c(
    as.numeric(test_delayed_g1_vs_g2$statistic),
    as.numeric(test_immediate_g1_vs_g2$statistic)
  ),
  df = c(
    as.numeric(test_delayed_g1_vs_g2$parameter),
    as.numeric(test_immediate_g1_vs_g2$parameter)
  ),
  p_value = c(
    test_delayed_g1_vs_g2$p.value,
    test_immediate_g1_vs_g2$p.value
  ),
  cohen_dz = c(
    d_z_delayed_g1_vs_g2,
    d_z_immediate_g1_vs_g2
  ),
  alternative = c(
    "two.sided",
    "two.sided"
  ),
  paired = c(
    TRUE,
    TRUE
  )
)

print(guess1_vs_guess2_results)

write.csv(
  guess1_vs_guess2_results,
  file = file.path(output_dir, "guess1_vs_guess2_results.csv"),
  row.names = FALSE
)

cat("Guess 1 vs Guess 2 results saved to:",
    file.path(output_dir, "guess1_vs_guess2_results.csv"),
    "\n")