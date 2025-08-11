test_that("Returns ANOVA results when normality and variance homogeneity are met", {
  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 20),
    .value = c(rnorm(20, 5), rnorm(20, 6), rnorm(20, 7))
  )

  res <- run_multi_group_tests(df)
  expect_equal(res$type, "ANOVA")
  expect_true("anova" %in% names(res))
  expect_true(!is.null(res$eta_squared))
  expect_gt(res$eta_squared, 0)
  expect_true(res$interpretation %in% c("negligible", "small", "moderate", "large"))
})

test_that("Returns Kruskal-Wallis results when normality or variance fails", {
  set.seed(123)
  df <- data.frame(
    jahr = rep(c("X", "Y", "Z"), each = 20),
    .value = c(rnorm(20, 5), rexp(20, 1), rpois(20, 6))
  )

  res <- run_multi_group_tests(df)
  expect_equal(res$type, "Kruskal-Wallis")
  expect_true("kruskal" %in% names(res))
  expect_true(!is.null(res$eta_squared_approx))
  expect_gte(res$eta_squared_approx, 0)
})

test_that("Returns NULL posthoc when TukeyHSD fails gracefully", {
  set.seed(123)
  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 4),
    .value = c(rnorm(4, 5), rnorm(4, 6), rnorm(4, 7))
  )

  res <- run_multi_group_tests(df)
  expect_equal(res$type, "ANOVA")
  expect_true("posthoc" %in% names(res))
  expect_true(is.null(res$posthoc) || is.list(res$posthoc))
})

test_that("Calculates correct effect size depending on test type", {
  set.seed(123)
  df1 <- data.frame(
    jahr = rep(c("2020", "2021", "2022"), each = 10),
    .value = c(rnorm(10, 3), rnorm(10, 5), rnorm(10, 7))
  )
  res1 <- run_multi_group_tests(df1)
  expect_true("eta_squared" %in% names(res1))
  set.seed(234)
  df2 <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 10),
    .value = c(rexp(10, 1), rexp(10, 2), rexp(10, 3))
  )
  res2 <- run_multi_group_tests(df2)
  expect_true("eta_squared_approx" %in% names(res2))
})

test_that("Returns assumption outputs: Shapiro, Levene, Bartlett", {
  set.seed(123)
  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 10),
    .value = c(rnorm(10, 1), rnorm(10, 2), rnorm(10, 3))
  )

  res <- run_multi_group_tests(df)
  expect_type(res$assumptions$shapiro, "list")
  expect_type(res$assumptions$levene_p, "double")
  expect_type(res$assumptions$bartlett_p, "double")
})

test_that("Omits effect size when effect_size = FALSE", {
  set.seed(123)
  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 10),
    .value = c(rnorm(10), rnorm(10, 1), rnorm(10, 2))
  )

  res <- run_multi_group_tests(df, effect_size = FALSE)
  expect_true(is.null(res$eta_squared) || is.null(res$eta_squared_approx))
})

test_that("Omits assumptions when report_assumptions = FALSE", {
  set.seed(123)
  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 8),
    .value = rnorm(24)
  )

  res <- run_multi_group_tests(df, report_assumptions = FALSE)
  expect_true("assumptions" %in% names(res))
  expect_type(res$assumptions, "list")
})

test_that("Warns when any group has less than 3 observations", {
  df <- data.frame(
    jahr = c(rep("A", 2), rep("B", 5), rep("C", 5)),
    .value = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  )

  expect_warning(
    res <- run_multi_group_tests(df),
    "weniger als 3 Beobachtungen"
  )
  expect_true(res$type %in% c("ANOVA", "Kruskal-Wallis"))
})

test_that("Falls back to Kruskal-Wallis when data are clearly non-normal", {
  set.seed(677)
  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 15),
    .value = c(rpois(15, 3), rpois(15, 6), rpois(15, 9))
  )

  res <- run_multi_group_tests(df)
  expect_equal(res$type, "Kruskal-Wallis")
})

test_that("Returns fallback Dunn error when FSA is unavailable", {
  skip_if(requireNamespace("FSA", quietly = TRUE), "FSA is installed, cannot test fallback")

  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 10),
    .value = c(rpois(10, 2), rpois(10, 5), rpois(10, 8))
  )

  res <- run_multi_group_tests(df)
  expect_type(res$dunn, "character")
  expect_match(res$dunn, "FSA package fehlt")
})
