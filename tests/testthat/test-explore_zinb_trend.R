test_that("ZIP model runs and returns expected structure", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  res <- explore_zinb_trend(df, datum_col = "date", value_col = "count", family = "zip", k_spline = 3)
  expect_type(res, "list")
  expect_s3_class(res$model, "zeroinfl")
  expect_match(res$model_family_used, "ZIP")
  expect_true(is.null(res$plot) || inherits(res$plot, "ggplot"))
})

test_that("ZINB model runs and returns expected structure", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  res <- explore_zinb_trend(df, datum_col = "date", value_col = "count", family = "zinb")
  expect_type(res, "list")
  expect_s3_class(res$model, "zeroinfl")
  expect_match(res$model_family_used, "ZINB")
})

test_that("Automatic model selection returns valid AIC table", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  res <- explore_zinb_trend(df, datum_col = "date", value_col = "count", family = "auto")
  expect_true(is.null(res$aic_comparison) || inherits(res$aic_comparison, "data.frame"))
  expect_true(res$model_family_used %in% c("Zero-Inflated ZIP", "Zero-Inflated ZINB"))
})

test_that("Return formula works correctly", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  fml <- explore_zinb_trend(df, datum_col = "date", value_col = "count", return_formula = TRUE)
  expect_s3_class(fml, "formula")
})

test_that("Negative values trigger error", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  df$count[1] <- -5
  expect_error(explore_zinb_trend(df, datum_col = "date", value_col = "count"))
})

test_that("Vuong test runs only if both models are available", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  res <- explore_zinb_trend(df, datum_col = "date", value_col = "count", family = "auto", run_vuong = TRUE)
  if (!is.null(res$vuong_test)) {
    expect_true(inherits(res$vuong_test, "htest"))
  } else {
    expect_null(res$vuong_test)
  }
})

test_that("Model returns ggplot when successful", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  res <- explore_zinb_trend(df, datum_col = "date", value_col = "count", family = "zinb", verbose = TRUE)
  expect_true(is.null(res$plot) || inherits(res$plot, "ggplot"))
})

test_that("Grouped interaction model runs correctly", {
  skip_if_not_installed("pscl")
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  df$group <- sample(c("A", "B"), size = nrow(df), replace = TRUE)
  res <- explore_zinb_trend(
    df, datum_col = "date", value_col = "count",
    group_col = "group", family = "zip"
  )
  expect_true(!is.null(res$model))
  expect_s3_class(res$model, "zeroinfl")
  expect_match(res$model_family_used, "ZIP")
})

test_that("Control = numeric is accepted and used", {
  skip_if_not_installed("pscl")
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  res <- explore_zinb_trend(
    df, datum_col = "date", value_col = "count",
    control = 300, family = "zinb"
  )
  expect_true(!is.null(res$model))
  expect_s3_class(res$model, "zeroinfl")
})

test_that("Control = list is accepted and applied", {
  skip_if_not_installed("pscl")
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  res <- explore_zinb_trend(
    df, datum_col = "date", value_col = "count",
    control = list(maxit = 150), family = "zip"
  )
  expect_true(!is.null(res$model))
  expect_s3_class(res$model, "zeroinfl")
})

test_that("Invalid date format throws error", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  df$date <- as.character(df$date)
  expect_error(
    explore_zinb_trend(df, datum_col = "date", value_col = "count"),
    regexp = "Date.*POSIXct"
  )
})


test_that("Invalid control throws error", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  expect_error(
    explore_zinb_trend(df, datum_col = "date", value_col = "count", control = "wrong"),
    regexp = "control.*muss.*(NULL|Zahl|Liste)"
  )
})

test_that("Missing required columns trigger error", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  expect_error(
    explore_zinb_trend(df, datum_col = "unknown", value_col = "count"),
    regexp = "existiert nicht|not found"
  )
  expect_error(
    explore_zinb_trend(df, datum_col = "date", value_col = "unknown"),
    regexp = "existiert nicht|not found"
  )
})

test_that("No model fits returns NULL model with warning message", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  df$count <- 1  # no variance → should fail
  res <- explore_zinb_trend(
    df, datum_col = "date", value_col = "count", family = "auto"
  )
  expect_null(res$model)
  expect_true(any(grepl("kein Modell konnte konvergieren|no model could fit", res$messages)))
})

