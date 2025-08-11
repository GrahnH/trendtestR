test_that("Handles 2-group continuous data (default unpaired)", {
  df <- data.frame(
    jahr = rep(c("A", "B"), each = 5),
    .value = c(rnorm(5, 5), rnorm(5, 6))
  )
  attr(df, "value_data_type") <- "continuous"

  res <- run_group_tests(df)
  expect_equal(res$type, "Unpaired Test")
  expect_true("t_test" %in% names(res))
})

test_that("Handles 2-group continuous data with paired = TRUE", {
  df <- data.frame(
    jahr = rep(c("pre", "post"), each = 6),
    .value = rnorm(12)
  )
  attr(df, "value_data_type") <- "continuous"

  res <- run_group_tests(df, paired = TRUE)
  expect_equal(res$type, "Paired Test")
  expect_true("t_test" %in% names(res))
})

test_that("Handles 3-group continuous data (ANOVA or Kruskal)", {
  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 7),
    .value = c(rnorm(7, 3), rnorm(7, 4), rnorm(7, 5))
  )
  attr(df, "value_data_type") <- "continuous"

  res <- run_group_tests(df)
  expect_true(res$type %in% c("ANOVA", "Kruskal-Wallis"))
  expect_true(any(c("anova", "kruskal") %in% names(res)))
})

test_that("Handles 2-group count data", {
  df <- data.frame(
    jahr = rep(c("A", "B"), each = 6),
    .value = rpois(12, 5)
  )
  attr(df, "value_data_type") <- "count"

  res <- run_group_tests(
    df = df,
    value_col = ".value",
    group_col = "jahr"
  )

  print(res$type)
  expect_true(grepl("Count|Poisson|Negative Binomial", res$type, ignore.case = TRUE))
  expect_true("sample_sizes" %in% names(res))
})

test_that("Handles 3-group count data", {
  df <- data.frame(
    jahr = rep(c("X", "Y", "Z"), each = 6),
    .value = rpois(18, lambda = 8)
  )
  attr(df, "value_data_type") <- "count"

  res <- run_group_tests(df)
  expect_true(grepl("Count|Poisson|Negative Binomial", res$type, ignore.case = TRUE))
})

test_that("Handles missing 'value_data_type' attribute and infers it", {
  df <- data.frame(
    jahr = rep(c("A", "B"), each = 10),
    .value = c(rnorm(10, 10), rnorm(10, 12))
  )

  res <- run_group_tests(df)
  expect_true(res$type %in% c("Unpaired Test", "Paired Test"))
  expect_true("t_test" %in% names(res))
})

test_that("Handles < 2 groups with proper message", {
  df <- data.frame(
    jahr = rep("A", 5),
    .value = rnorm(5)
  )
  attr(df, "value_data_type") <- "continuous"

  res <- run_group_tests(df)
  expect_equal(res$type, "Nicht unterstuetzt")
  expect_equal(res$group_count, 1)
  expect_equal(res$error, "Weniger als 2 Gruppen")
})

test_that("Handles NA values and still returns results", {
  df <- data.frame(
    jahr = rep(c("A", "B"), each = 6),
    .value = c(rnorm(5, 5), NA, rnorm(6, 6))
  )
  attr(df, "value_data_type") <- "continuous"

  res <- run_group_tests(df)
  expect_equal(res$type, "Unpaired Test")
})

test_that("Falls back to Kruskal-Wallis when data are clearly non-normal", {
  set.seed(123)
  df <- data.frame(
    jahr = rep(c("A", "B", "C"), each = 30),
    .value = c(
      rexp(30, rate = 1),
      rnorm(30, mean = 10),
      runif(30, min = 2, max = 4)
    )
  )

  res <- run_multi_group_tests(df)
  expect_equal(res$type, "Kruskal-Wallis")
})

test_that("prints Hinweis for binary when attribute is preset", {
  df <- data.frame(
    jahr   = rep("2020", 6),
    .value = c(0, 1, 1, 0, 1, 0)
  )
  attr(df, "value_data_type") <- "binary"
  msgs <- testthat::capture_messages(run_group_tests(df))
  expect_true(any(grepl("Variablentyp 'binary'", msgs)))
})
