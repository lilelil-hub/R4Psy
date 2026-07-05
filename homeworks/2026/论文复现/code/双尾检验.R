##@@ ONE-TAILED VS TWO-TAILED CHECK @@##

# Delayed condition: Session 1 vs Mean
test_delayed_s1_two <- t.test(
  data.delayed$mse.s1,
  data.delayed$mse.mean,
  alternative = "two.sided",
  paired = TRUE
)

# Delayed condition: Session 2 vs Mean
test_delayed_s2_two <- t.test(
  data.delayed$mse.s2,
  data.delayed$mse.mean,
  alternative = "two.sided",
  paired = TRUE
)

# Immediate condition: Guess 1 vs Mean
test_immediate_g1_two <- t.test(
  data.immediate$mse.g1,
  data.immediate$mse.mean,
  alternative = "two.sided",
  paired = TRUE
)

# Immediate condition: Guess 2 vs Mean
test_immediate_g2_two <- t.test(
  data.immediate$mse.g2,
  data.immediate$mse.mean,
  alternative = "two.sided",
  paired = TRUE
)

two_tailed_results <- data.frame(
  analysis = c(
    "Delayed: mse.s1 vs mse.mean",
    "Delayed: mse.s2 vs mse.mean",
    "Immediate: mse.g1 vs mse.mean",
    "Immediate: mse.g2 vs mse.mean"
  ),
  N = c(
    nrow(data.delayed),
    nrow(data.delayed),
    nrow(data.immediate),
    nrow(data.immediate)
  ),
  t_value = c(
    as.numeric(test_delayed_s1_two$statistic),
    as.numeric(test_delayed_s2_two$statistic),
    as.numeric(test_immediate_g1_two$statistic),
    as.numeric(test_immediate_g2_two$statistic)
  ),
  df = c(
    as.numeric(test_delayed_s1_two$parameter),
    as.numeric(test_delayed_s2_two$parameter),
    as.numeric(test_immediate_g1_two$parameter),
    as.numeric(test_immediate_g2_two$parameter)
  ),
  p_value_two_tailed = c(
    test_delayed_s1_two$p.value,
    test_delayed_s2_two$p.value,
    test_immediate_g1_two$p.value,
    test_immediate_g2_two$p.value
  ),
  significant_05 = c(
    test_delayed_s1_two$p.value < .05,
    test_delayed_s2_two$p.value < .05,
    test_immediate_g1_two$p.value < .05,
    test_immediate_g2_two$p.value < .05
  )
)

print(two_tailed_results)

write.csv(
  two_tailed_results,
  file = file.path(output_dir, "two_tailed_check_results.csv"),
  row.names = FALSE
)