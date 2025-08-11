test_that("Returns valid result and p-value for clean two-group Poisson data", {
  df <- data.frame(
    jahr = rep(c("2020", "2021"), each = 10),
    .value = c(rpois(10, 5), rpois(10, 8))
  )
  res <- run_count_two_group_tests(df)
  expect_type(res, "list")
  expect_equal(length(res$group_names), 2)
  expect_equal(res$type, "Poisson Regression")
  expect_true(!is.na(res$p_value))
  expect_match(res$direction, "hoeher|aehnlich")
})

test_that("Triggers Negative Binomial regression when overdispersion is detected", {
  set.seed(123)
  df <- data.frame(
    jahr = rep(c("2020", "2021"), each = 20),
    .value = c(rpois(20, 5), rnbinom(20, mu = 20, size = 0.5))
  )
  res <- run_count_two_group_tests(df, phi = 1.1)
  expect_match(res$type, "Negative Binomial")
  expect_true(res$assumption_status$is_overdispersed)
})

test_that("Returns IRR effect size when requested", {
  df <- data.frame(
    jahr = rep(c("A", "B"), each = 20),
    .value = c(rpois(20, 2), rpois(20, 10))
  )
  res <- run_count_two_group_tests(df, effect_size = TRUE)
  expect_true("effect_size" %in% names(res))
  expect_true(res$effect_size$value > 1)
  expect_true("IRR" %in% names(res$confidence_intervals))
})

test_that("Skips assumption report when report_assumptions = FALSE", {
  df <- data.frame(
    jahr = rep(c("X", "Y"), each = 10),
    .value = rpois(20, 5)
  )
  res <- run_count_two_group_tests(df, report_assumptions = FALSE)
  expect_equal(res$assumption_status, list())
})

test_that("Converts non-factor group column to factor with message", {
  df <- data.frame(
    jahr = rep(2020:2021, each = 10),
    .value = rpois(20, 6)
  )
  expect_type(df$jahr, "integer")
  expect_message(run_count_two_group_tests(df), "Faktor umgewandelt")
})

test_that("Removes negative values and issues warning", {
  df <- data.frame(
    jahr = rep(c("2020", "2021"), each = 10),
    .value = c(-1, rpois(9, 4), rpois(10, 6))
  )
  expect_warning(
    res <- run_count_two_group_tests(df),
    "Negative Werte in Zaehldaten"
  )
  expect_true(all(res$basic_stats$sample_sizes >= 9))
})

test_that("Throws error when only one group is present", {
  df <- data.frame(
    jahr = rep("only", 10),
    .value = rpois(10, 5)
  )
  expect_error(
    run_count_two_group_tests(df),
    "Nicht genuegend Gruppen"
  )
})

test_that("Returns NA and warning when more than two groups are present", {
  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 5),
    .value = rpois(15, 5)
  )
  expect_warning(
    res <- run_count_two_group_tests(df),
    "Nicht genau zwei Gruppen"
  )
  expect_true(is.na(res$p_value))
})

test_that("Handles model fitting failure gracefully", {
  df <- data.frame(
    jahr = rep(c("X", "Y"), each = 10),
    .value = rep(NA, 20)
  )
  res <- run_count_two_group_tests(df)
  expect_true(is.null(res$p_value) || is.na(res$p_value))
  expect_true(grepl("Fehler", res$status) || is.na(res$p_value))
})

test_that("Returns confidence intervals for group coefficient if model fits", {
  set.seed(123)
  df <- data.frame(
    jahr = rep(c("X", "Y"), each = 15),
    .value = c(rpois(15, 2), rpois(15, 10))
  )
  res <- run_count_two_group_tests(df, effect_size = TRUE)
  expect_true(!is.null(res$confidence_intervals$group_coefficient))
  expect_true(!is.null(res$confidence_intervals$IRR))
})

test_that("Returns NA IRR when group coefficient is not estimable", {
   set.seed(123)
   df <- data.frame(
    jahr = rep(c("X", "Y"), each = 10),
    .value = rep(5, 20)
  )
  res <- run_count_two_group_tests(df, effect_size = TRUE)
  expect_true(is.na(res$effect_size$value) || res$p_value > 0.9)
})
