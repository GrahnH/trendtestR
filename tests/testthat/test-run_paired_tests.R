prepare_group_data <- function(df, value_col, group_col) {
  groups <- unique(df[[group_col]])
  vals <- list()
  for (group in groups) {
    vals[[as.character(group)]] <- df[df[[group_col]] == group, value_col]
  }
  return(list(vals = vals))
}

test_that("Returns paired t-test results when normality is met", {
  set.seed(123)
  df <- data.frame(
    jahr = rep(c("2020", "2021"), each = 20),
    .value = c(rnorm(20, 30, 5), rnorm(20, 32, 5))
  )
  result <- run_paired_tests(df, paired = TRUE)

  expect_equal(result$type, "Paired Test")
  expect_equal(result$sample_sizes, c(20, 20))
  expect_true("t_test" %in% names(result))
  expect_true("wilcox_test" %in% names(result))
  expect_true("effect_size" %in% names(result))
  expect_true("assumptions" %in% names(result))
  expect_true(grepl("t-test", result$recommendation))
})

test_that("Returns unpaired test results with different sample sizes", {
  set.seed(124)
  df <- data.frame(
    group = rep(c("A", "B"), c(15, 18)),
    values = c(rnorm(15, 25, 4), rnorm(18, 28, 4))
  )
  result <- run_paired_tests(df, value_col = "values", group_col = "group", paired = FALSE)

  expect_equal(result$type, "Unpaired Test")
  expect_equal(result$sample_sizes, c(15, 18))
  expect_equal(result$group_col, "group")
  expect_true(all(c("A", "B") %in% result$group_names))
})

test_that("Returns Wilcoxon test recommendation when data are non-normal", {
  set.seed(129)
  df <- data.frame(
    jahr = rep(c("before", "after"), each = 25),
    .value = c(rexp(25, 0.1), rexp(25, 0.08))
  )
  result <- run_paired_tests(df, paired = TRUE, alpha = 0.05)

  expect_true(!is.null(result$recommendation))
  expect_true(grepl("Wilcoxon", result$recommendation))
})

test_that("Calculates Cohen's d for paired tests when effect_size = TRUE", {
  set.seed(127)
  df <- data.frame(
    jahr = rep(c("pre", "post"), each = 10),
    .value = c(rnorm(10, 40, 6), rnorm(10, 45, 6))
  )
  result <- run_paired_tests(df, effect_size = TRUE, paired = TRUE)

  expect_true(!is.null(result$effect_size))
  expect_true(!is.null(result$effect_size$cohens_d) || is.numeric(result$effect_size$cohens_d))
})

test_that("Skips effect size calculation when effect_size = FALSE", {
  set.seed(128)
  df <- data.frame(
    jahr = rep(c("T1", "T2"), each = 12),
    .value = c(rnorm(12, 60, 10), rnorm(12, 65, 10))
  )
  result <- run_paired_tests(df, effect_size = FALSE, paired = TRUE)

  expect_true(is.null(result$effect_size$cohens_d))
})

test_that("Returns assumption test results when report_assumptions = TRUE", {
  set.seed(128)
  df <- data.frame(
    jahr = rep(c("T1", "T2"), each = 12),
    .value = c(rnorm(12, 60, 10), rnorm(12, 65, 10))
  )
  result <- run_paired_tests(df, report_assumptions = TRUE, paired = TRUE)

  expect_true(!is.null(result$assumptions))
  expect_true(!is.null(result$assumptions$normality_test))
  expect_true(is.logical(result$assumptions$normality))
})

test_that("Skips assumption tests when report_assumptions = FALSE", {
  set.seed(128)
  df <- data.frame(
    jahr = rep(c("T1", "T2"), each = 12),
    .value = c(rnorm(12, 60, 10), rnorm(12, 65, 10))
  )
  result <- run_paired_tests(df, report_assumptions = FALSE, paired = TRUE)

  expect_true(is.null(result$assumptions))
})

test_that("Returns error when sample size is too small", {
  df <- data.frame(
    jahr = rep(c("2020", "2021"), each = 2),
    .value = c(1, 2, 3, 4)
  )
  result <- run_paired_tests(df, paired = TRUE)

  expect_true(grepl("Fehler", result$type))
  expect_true("error" %in% names(result))
  expect_true(grepl("weniger als 3", result$error))
})

test_that("Returns error when paired = TRUE but unequal sample sizes", {
  df <- data.frame(
    jahr = c(rep("A", 10), rep("B", 8)),
    .value = c(rnorm(10, 20), rnorm(8, 22))
  )
  result <- run_paired_tests(df, paired = TRUE)

  expect_true(grepl("Fehler", result$type))
  expect_true("error" %in% names(result))
  expect_equal(result$sample_sizes, c(10, 8))
  expect_true(grepl("Unterschiedliche Gruppengroessen", result$error))
})

test_that("Handles missing values gracefully", {
  set.seed(132)
  df <- data.frame(
    jahr = rep(c("2020", "2021"), each = 15),
    .value = c(rnorm(15, 40, 7), rnorm(15, 42, 7))
  )
  df$.value[c(3, 8, 18, 25)] <- NA
  result <- run_paired_tests(df, paired = FALSE)

  expect_true(!is.null(result$type))
  expect_true(!is.null(result$t_test))
  expect_true(!is.null(result$wilcox_test))
})

test_that("Handles extreme values and recommends non-parametric test", {
  set.seed(133)
  df <- data.frame(
    jahr = rep(c("low", "high"), each = 10),
    .value = c(rep(1, 9), 1000, rep(100, 9), 10000)
  )
  result <- run_paired_tests(df, paired = FALSE)

  expect_true(!is.null(result$wilcox_test))
  expect_true(grepl("Mann-Whitney|Wilcoxon", result$recommendation, ignore.case = TRUE))
})

test_that("Handles zero variance data without crashing", {
  df <- data.frame(
    jahr = rep(c("A", "B"), each = 10),
    .value = rep(50, 20)
  )
  result <- run_paired_tests(df, paired = TRUE)

  expect_true(!is.null(result$type))
  expect_true(is.null(result$effect_size$cohens_d) ||
                is.na(result$effect_size$cohens_d) ||
                result$effect_size$cohens_d == 0)
})

test_that("Works with custom column names", {
  set.seed(125)
  df <- data.frame(
    category = rep(c("before", "after"), each = 12),
    measurement = c(rnorm(12, 100, 15), rnorm(12, 105, 15))
  )
  result <- run_paired_tests(df, value_col = "measurement", group_col = "category", paired = TRUE)

  expect_equal(result$group_col, "category")
  expect_true(all(c("before", "after") %in% result$group_names))
})

test_that("Respects different alpha levels for normality testing", {
  set.seed(126)
  df <- data.frame(
    jahr = rep(c("2020", "2021"), each = 15),
    .value = c(rnorm(15, 50, 8), rnorm(15, 52, 8))
  )
  result1 <- run_paired_tests(df, alpha = 0.01, paired = TRUE)
  result2 <- run_paired_tests(df, alpha = 0.10, paired = TRUE)

  expect_true(!is.null(result1$assumptions))
  expect_true(!is.null(result2$assumptions))
  expect_true(is.character(result1$recommendation))
  expect_true(is.character(result2$recommendation))
})

test_that("Calculates non-parametric effect sizes for unpaired tests", {
  set.seed(131)
  df <- data.frame(
    group = rep(c("X", "Y"), c(12, 15)),
    value = c(rnorm(12, 30, 5), rnorm(15, 35, 5))
  )
  result <- run_paired_tests(df, value_col = "value", group_col = "group", paired = FALSE, effect_size = TRUE)

  expect_equal(result$type, "Unpaired Test")
  expect_true(!is.null(result$effect_size))
  expect_true(!is.null(result$effect_size$nonparam_r) || !is.null(result$effect_size$rank_biserial))
})

test_that("Returns complete result structure with all parameters enabled", {
  set.seed(130)
  df <- data.frame(
    treatment = rep(c("control", "treatment"), each = 20),
    response = c(rnorm(20, 75, 12), rnorm(20, 80, 12))
  )
  result <- run_paired_tests(
    df = df,
    value_col = "response",
    group_col = "treatment",
    alpha = 0.01,
    effect_size = TRUE,
    report_assumptions = TRUE,
    paired = TRUE
  )

  expected_components <- c("type", "sample_sizes", "group_col", "group_names",
                           "t_test", "wilcox_test", "effect_size",
                           "assumptions", "recommendation")

  for (component in expected_components) {
    expect_true(component %in% names(result))
  }
})
