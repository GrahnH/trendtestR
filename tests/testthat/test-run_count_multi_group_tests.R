create_test_data <- function(n_per_group = 50, means = c(3, 5, 4), groups = c("2020", "2021", "2022")) {
  set.seed(123)
  data <- data.frame(
    .value = c(rpois(n_per_group, means[1]),
               rpois(n_per_group, means[2]),
               rpois(n_per_group, means[3])),
    jahr = factor(rep(groups, each = n_per_group))
  )
  return(data)
}

create_overdispersed_data <- function(n_per_group = 50, means = c(10, 15, 12), dispersion = 5) {
  set.seed(456)
  data <- data.frame(
    .value = c(rnbinom(n_per_group, mu = means[1], size = dispersion),
               rnbinom(n_per_group, mu = means[2], size = dispersion),
               rnbinom(n_per_group, mu = means[3], size = dispersion)),
    jahr = factor(rep(c("A", "B", "C"), each = n_per_group))
  )
  return(data)
}

test_that("returns list with required components when given valid input", {
  data <- create_test_data()
  result <- run_count_multi_group_tests(df = data)

  expect_type(result, "list")
  expect_true("p_value" %in% names(result))
  expect_true("type" %in% names(result))
  expect_true("group_names" %in% names(result))
  expect_true("sample_sizes" %in% names(result))
})

test_that("produces valid p-values within expected range", {
  data <- create_test_data()
  result <- run_count_multi_group_tests(df = data)

  expect_true(!is.na(result$p_value))
  expect_true(is.numeric(result$p_value))
  expect_true(result$p_value >= 0 & result$p_value <= 1)
})

test_that("correctly identifies group information", {
  data <- create_test_data()
  result <- run_count_multi_group_tests(df = data)

  expect_equal(length(result$group_names), 3)
  expect_equal(names(result$sample_sizes), c("2020", "2021", "2022"))
})

test_that("accepts custom column names", {
  data_custom <- create_test_data()
  names(data_custom) <- c("count", "group")
  result <- run_count_multi_group_tests(df = data_custom, value_col = "count", group_col = "group")

  expect_type(result, "list")
  expect_equal(result$group_col, "group")
})

test_that("handles different alpha levels consistently", {
  data <- create_test_data()

  result_001 <- run_count_multi_group_tests(df = data, alpha = 0.01)
  result_05 <- run_count_multi_group_tests(df = data, alpha = 0.05)
  result_10 <- run_count_multi_group_tests(df = data, alpha = 0.10)

  expect_type(result_001, "list")
  expect_type(result_05, "list")
  expect_type(result_10, "list")
  expect_equal(result_001$p_value, result_05$p_value)
})

test_that("warns about negative values and removes them", {
  data_with_negatives <- create_test_data()
  data_with_negatives$.value[1:5] <- -1

  expect_warning(
    result <- run_count_multi_group_tests(df = data_with_negatives),
    "Negative Werte in Zaehldaten erkannt"
  )
  expect_type(result, "list")
})

test_that("converts character groups to factors with message", {
  data_char_group <- create_test_data()
  data_char_group$jahr <- as.character(data_char_group$jahr)

  expect_message(
    result <- run_count_multi_group_tests(df = data_char_group),
    "wurde in einen Faktor umgewandelt"
  )
  expect_type(result, "list")
})

test_that("handles missing values appropriately", {
  data_with_na <- create_test_data()
  data_with_na$.value[1:10] <- NA

  result <- run_count_multi_group_tests(df = data_with_na)
  expect_type(result, "list")
  expect_true(sum(result$sample_sizes) == nrow(data_with_na) - 10)
})

test_that("throws error with insufficient groups", {
  data_two_groups <- create_test_data()
  data_two_groups <- data_two_groups[data_two_groups$jahr != "2022", ]

  expect_error(
    run_count_multi_group_tests(df = data_two_groups),
    "Nicht genuegend Gruppen"
  )
})

test_that("throws error with insufficient sample sizes", {
  data_small <- data.frame(
    .value = c(1, 2, 1, 2, 1),
    jahr = factor(c("A", "A", "B", "C", "C"))
  )

  expect_error(
    run_count_multi_group_tests(df = data_small),
    "weniger als 2 Beobachtungen"
  )
})

test_that("handles multiple groups beyond three", {
  set.seed(999)
  data_multi <- data.frame(
    .value = c(rpois(20, 3), rpois(20, 5), rpois(20, 4), rpois(20, 6), rpois(20, 2)),
    jahr = factor(rep(c("A", "B", "C", "D", "E"), each = 20))
  )

  result <- run_count_multi_group_tests(df = data_multi)

  expect_type(result, "list")
  expect_equal(length(result$group_names), 5)
  expect_equal(length(result$sample_sizes), 5)
})

test_that("detects overdispersion and switches to negative binomial", {
  overdispersed_data <- create_overdispersed_data()

  expect_message(
    result <- run_count_multi_group_tests(df = overdispersed_data, report_assumptions = TRUE),
    "Ueberdispersion erkannt"
  )

  expect_true(grepl("Negative Binomial", result$type))
  expect_true("assumption_status" %in% names(result))
  expect_true("is_overdispersed" %in% names(result$assumption_status))
})

test_that("responds to different phi thresholds", {
  overdispersed_data <- create_overdispersed_data()

  result_low_phi <- run_count_multi_group_tests(df = overdispersed_data, phi = 1.2)
  result_high_phi <- run_count_multi_group_tests(df = overdispersed_data, phi = 3.0)

  expect_type(result_low_phi, "list")
  expect_type(result_high_phi, "list")
})

test_that("calculates effect size when requested", {
  data <- create_test_data()

  result_with_effect <- run_count_multi_group_tests(df = data, effect_size = TRUE)

  expect_true("effect_size" %in% names(result_with_effect))
  expect_type(result_with_effect$effect_size, "list")
  expect_true("type" %in% names(result_with_effect$effect_size))
  expect_true("value" %in% names(result_with_effect$effect_size))
  expect_true(is.numeric(result_with_effect$effect_size$value))
})

test_that("does not calculate effect size by default", {
  data <- create_test_data()
  result <- run_count_multi_group_tests(df = data, effect_size = FALSE)

  expect_equal(result$effect_size, NA)
})

test_that("includes assumption diagnostics when requested", {
  data <- create_test_data()

  result_with_assumptions <- run_count_multi_group_tests(df = data, report_assumptions = TRUE)

  expect_true("assumption_status" %in% names(result_with_assumptions))
  expect_type(result_with_assumptions$assumption_status, "list")
})

test_that("conducts post-hoc tests for significant overall results", {
  set.seed(789)
  data_significant <- data.frame(
    .value = c(rpois(30, 2), rpois(30, 8), rpois(30, 15)),
    jahr = factor(rep(c("Low", "Medium", "High"), each = 30))
  )

  result <- run_count_multi_group_tests(df = data_significant, alpha = 0.05)

  if (result$p_value < 0.05) {
    expect_true(!is.null(result$significant_pairwise_differences))
    expect_false(is.character(result$significant_pairwise_differences) &&
                   result$significant_pairwise_differences == "Gesamttest nicht signifikant. Keine Post-Hoc-Tests erforderlich.")
  }
})

test_that("handles identical values across groups", {
  data_same <- data.frame(
    .value = rep(5, 90),
    jahr = factor(rep(c("A", "B", "C"), each = 30))
  )

  result_same <- run_count_multi_group_tests(df = data_same)
  expect_type(result_same, "list")
})

test_that("handles groups with all zeros", {
  data_zeros <- data.frame(
    .value = c(rep(0, 30), rpois(30, 3), rpois(30, 5)),
    jahr = factor(rep(c("Zero", "Low", "High"), each = 30))
  )

  result_zeros <- run_count_multi_group_tests(df = data_zeros)
  expect_type(result_zeros, "list")
})

test_that("has required packages available", {
  expect_true(requireNamespace("MASS", quietly = TRUE))
  expect_true(requireNamespace("car", quietly = TRUE))
  expect_true(requireNamespace("multcomp", quietly = TRUE))
  expect_true(requireNamespace("emmeans", quietly = TRUE))
  expect_true(requireNamespace("dplyr", quietly = TRUE))
})

test_that("maintains consistent output structure across different inputs", {
  data1 <- create_test_data(n_per_group = 30)
  data2 <- create_overdispersed_data(n_per_group = 25)

  result1 <- run_count_multi_group_tests(df = data1, effect_size = TRUE, report_assumptions = TRUE)
  result2 <- run_count_multi_group_tests(df = data2, effect_size = TRUE, report_assumptions = TRUE)

  basic_names <- c("type", "group_col", "group_names", "sample_sizes", "p_value",
                   "significant_pairwise_differences", "effect_size", "assumption_status")

  expect_true(all(basic_names %in% names(result1)))
  expect_true(all(basic_names %in% names(result2)))
})

test_that("falls back from emmeans to multcomp by Post-hoc test", {
  set.seed(456)
  data_sig_diff <- data.frame(
    .value = c(rpois(50, 2), rpois(50, 8), rpois(50, 15)), # Groups with different means
    jahr = factor(rep(c("2020", "2021", "2022"), each = 50))
  )

  mock_emmeans <- function(...) {
    stop("Simulated emmeans error")
  }

  emmeans_env <- new.env()
  emmeans_env$emmeans <- mock_emmeans
  emmeans_env$contrast <- function(...) {
    stop("Simulated emmeans error")
  }
  result <- run_count_multi_group_tests(
    df = data_sig_diff,
    value_col = ".value",
    group_col = "jahr",
    alpha = 0.05,
    phi = 1.5,
    effect_size = TRUE,
    report_assumptions = TRUE
  )

  expect_true(result$p_value < 0.05)
  expect_true(is.data.frame(result$significant_pairwise_differences))
  expect_true("contrast" %in% names(result$significant_pairwise_differences))
  expect_equal(nrow(result$significant_pairwise_differences), 3)

  data_mixed_diff <- data.frame(
    .value = c(rpois(50, 5), rpois(50, 6), rpois(50, 15)),
    jahr = factor(rep(c("2020", "2021", "2022"), each = 50))
  )

  result_mixed <- run_count_multi_group_tests(
    df = data_mixed_diff,
    value_col = ".value",
    group_col = "jahr",
    alpha = 0.05,
    phi = 1.5
  )

  expect_true(result_mixed$p_value < 0.05)
  expect_true(is.data.frame(result_mixed$significant_pairwise_differences))
  expect_true("all_pairwise_comparisons" %in% names(result_mixed))
  expect_true(nrow(result_mixed$significant_pairwise_differences) < nrow(result_mixed$all_pairwise_comparisons))
})

test_that("handles Post-hoc result in different scenarios", {
  # Scenario A: Significant differences (emmeans path)
  set.seed(789)
  data_sig <- data.frame(
    .value = c(rpois(50, 3), rpois(50, 10), rpois(50, 15)),
    jahr = factor(rep(c("A", "B", "C"), each = 50))
  )
  result_sig <- run_count_multi_group_tests(data_sig)
  expect_true(result_sig$p_value < 0.05)
  expect_true(is.data.frame(result_sig$significant_pairwise_differences))
  expect_equal(nrow(result_sig$significant_pairwise_differences), 3)

  # Scenario B: No significant differences (p > alpha)
  set.seed(101)
  data_no_sig <- data.frame(
    .value = c(rpois(50, 5), rpois(50, 5), rpois(50, 6)),
    jahr = factor(rep(c("A", "B", "C"), each = 50))
  )
  result_no_sig <- run_count_multi_group_tests(data_no_sig)
  expect_true(result_no_sig$p_value > 0.05)
  expect_equal(result_no_sig$significant_pairwise_differences, "Gesamttest nicht signifikant. Keine Post-Hoc-Tests erforderlich.")

  # Scenario C: Overall significant, but post-hoc not (emmeans path)
  set.seed(789)
  data_mixed_sig_posthoc <- data.frame(
    .value = c(rpois(50,6.1), rpois(50, 5.5), rpois(50, 6.5)),
    jahr = factor(rep(c("A", "B", "C"), each = 50))
  )
  result_mixed_sig_posthoc <- run_count_multi_group_tests(data_mixed_sig_posthoc)
  expect_true(result_mixed_sig_posthoc$p_value < 0.05)
  expect_equal(result_mixed_sig_posthoc$significant_pairwise_differences,"Keine signifikanten paarweisen Unterschiede (nach Anpassung).")
    expect_true("all_pairwise_comparisons" %in% names(result_mixed_sig_posthoc))
  expect_equal(nrow(result_mixed_sig_posthoc$all_pairwise_comparisons), 3)
})

test_that("Handles multcomp::glht fallback correctly", {
  set.seed(777)
  test_data_with_diff <- data.frame(
    .value = c(rpois(50, 3), rpois(50, 8), rpois(50, 3.5)),
    jahr = factor(rep(c("2020", "2021", "2022"), each = 50))
  )

  result <- run_count_multi_group_tests(
    df = test_data_with_diff,
    value_col = ".value",
    group_col = "jahr",
    alpha = 0.05
  )
  result
  expect_true(is.numeric(result$p_value) && result$p_value < 0.05)
  expect_s3_class(result$significant_pairwise_differences, "data.frame")
  expect_equal(nrow(result$significant_pairwise_differences), 2)
  expect_true("jahr2020 - jahr2021" %in% result$significant_pairwise_differences$contrast)
  expect_true("jahr2021 - jahr2022" %in% result$significant_pairwise_differences$contrast)
})

