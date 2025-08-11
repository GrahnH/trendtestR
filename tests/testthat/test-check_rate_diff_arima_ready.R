test_that("returns full diagnostics and plots for valid input", {

  set.seed(123)
  vec <- cumsum(rnorm(60, mean = 0.2, sd = 1))
  date_vec <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = 60)

  result <- check_rate_diff_arima_ready(
    rate_diff_vec = vec,
    date_vec = date_vec,
    frequency = 52,
    plot_acf = TRUE,
    do_stl = TRUE
  )

  expect_type(result, "list")
  expect_s3_class(result$ts_data, "ts")
  expect_true(result$assessment$recommendation)
  expect_true("plot_acf" %in% names(result$plots))
  expect_s3_class(result$plots$plot_acf, "gg")
  expect_s3_class(result$plots$plot_pacf, "gg")
  expect_true(!is.null(result$adf$p.value))
  expect_true(!is.null(result$kpss$p.value))
})

test_that("returns early exit when no recommendation is made", {
  vec <- rep(0.1, 60)
  result <- check_rate_diff_arima_ready(rate_diff_vec = vec, frequency = 52)

  expect_false(result$assessment$recommendation)
  expect_null(result$adf)
  expect_null(result$kpss)
  expect_null(result$stl)
  expect_true(is.na(result$stationarity_assessment$is_likely_stationary))
})

test_that("fails on invalid input: non-numeric, Inf, or NA", {
  expect_error(check_rate_diff_arima_ready(rate_diff_vec = c(1, 2, NA, 4, 5, Inf)),
               "numerisch.*Inf")
  expect_error(check_rate_diff_arima_ready(rate_diff_vec = c("a", "b", "c")),
               "numerisch")
})

test_that("fails if fewer than 10 valid data points", {
  vec <- c(1:5, rep(NA, 5))
  expect_error(check_rate_diff_arima_ready(vec), "mindestens 10")
})

test_that("skips ACF, PACF, and STL plots when disabled", {
  set.seed(999)
  vec <- rnorm(40)
  result <- check_rate_diff_arima_ready(
    rate_diff_vec = vec,
    plot_acf = FALSE,
    do_stl = FALSE,
    frequency = 12
  )

  expect_type(result$plots, "list")
  expect_null(result$plots$plot_acf)
  expect_null(result$plots$plot_pacf)
  expect_null(result$stl)
})

test_that("identifys and plotts Outliers", {
  set.seed(999)
  vec_with_outliers <- c(rnorm(50, 0, 1), 10, -12, rnorm(50, 0, 1))
  date_vec <- seq.Date(from = as.Date("2020-01-01"), by = "week", length.out = length(vec_with_outliers))

  res <- check_rate_diff_arima_ready(rate_diff_vec = vec_with_outliers, date_vec = date_vec, plot_acf = TRUE, do_stl = FALSE)
  expect_s3_class(res$plots$plot_timeseries, "ggplot")
  expect_equal(length(res$plots$plot_timeseries$layers), 3)

  set.seed(999)
  vec_without_outliers <- rnorm(100, 0, 1)
  res_no_outliers <- check_rate_diff_arima_ready(rate_diff_vec = vec_without_outliers, date_vec = date_vec, plot_acf = FALSE, do_stl = FALSE)
  expect_equal(length(res_no_outliers$plots$plot_timeseries$layers), 2)
})

test_that("returns expected output when STL decomposition works", {
  vec_seasonal <- ts(rep(c(1, 2, 3, 2), 25) + rnorm(100, 0, 0.1), frequency = 4)
  res <- check_rate_diff_arima_ready(vec_seasonal, frequency = 4, plot_acf = FALSE, do_stl = TRUE)

  expect_true(!is.null(res$stl))
  expect_true("seasonal" %in% colnames(res$stl$time.series))
  expect_true("stl" %in% names(res))
})
