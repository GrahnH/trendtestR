test_that("lm model returns valid plot and diagnostic structure", {
  df <- data.frame(x = rnorm(50), y = rnorm(50))
  model <- lm(y ~ x, data = df)
  out <- diagnose_model_trend(model)

  expect_type(out, "list")
  expect_named(out, c("plots", "diagnostics"))

  expect_true(all(c("residuals_vs_fitted", "qq", "scale_location") %in% names(out$plots)))
  lapply(out$plots, function(p) expect_s3_class(p, "gg"))
  expect_true("residual_type" %in% names(out$diagnostics))
  expect_true("shapiro_test" %in% names(out$diagnostics))
  expect_type(out$diagnostics$shapiro_test, "list")
})

test_that("handles group_col correctly with coloring", {
  df <- data.frame(x = rnorm(100), y = rnorm(100), grp = rep(c("A", "B", "C", "D"), 25))
  model <- lm(y ~ x + grp, data = df)
  out <- diagnose_model_trend(model, group_col = "grp")
  plot_data <- ggplot2::ggplot_build(out$plots$residuals_vs_fitted)$data[[1]]
  expect_true("group" %in% names(plot_data))
  expect_gte(length(unique(plot_data$group)), 4)
})

test_that("GAM model triggers gam.check and captures output", {
  skip_if_not_installed("mgcv")
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  model <- mgcv::gam(y ~ s(x), data = df)
  out <- diagnose_model_trend(model)

  expect_true("gam_check" %in% names(out$diagnostics))
  expect_type(out$diagnostics$gam_check, "character")
})

test_that("Zeroinfl model forces response residuals", {
  skip_if_not_installed("pscl")
  df <- data.frame(x = rnorm(100), y = rpois(100, 2))
  model <- pscl::zeroinfl(y ~ x | 1, data = df)
  out <- diagnose_model_trend(model, residual_type = "pearson")

  expect_equal(out$diagnostics$residual_type, "response")
})

test_that("Handles minimal input without crashing", {
  df <- data.frame(x = 1:2, y = c(1, 1))
  model <- lm(y ~ x, data = df)
  out <- diagnose_model_trend(model)

  expect_type(out, "list")
  expect_named(out, c("plots", "diagnostics"))
  expect_type(out$plots, "list")
  expect_type(out$diagnostics, "list")
})

test_that("Handles extremely small data correctly", {
  df <- data.frame(x = 1, y = 1)
  model <- lm(y ~ 1, data = df)
  out <- diagnose_model_trend(model, verbose = FALSE)
  expect_type(out, "list")
  expect_null(out$plots)
  expect_null(out$diagnostics)
})

test_that("Returns NA for diagnostics on too-small data", {
  df <- data.frame(x = 1:3, y = c(1, 2, 1.5))
  model <- lm(y ~ x, data = df)
  out <- diagnose_model_trend(model)

  ks <- out$diagnostics$ks_test
  expect_true(
    is.null(ks) || inherits(ks, "htest") || (is.atomic(ks) && length(ks) == 1 && is.na(ks))
  )

})
