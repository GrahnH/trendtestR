test_that("plot_weekly_cases() works with default settings", {
  set.seed(123)
  df <- data.frame(
    datum = as.Date("2022-01-01") + 0:100,
    neue_faelle = rpois(101, lambda = 20)
  )

  result <- plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = 10)

  expect_type(result, "list")
  expect_named(result, c("data", "trend_plot", "hist_plot", "box_plot"))
  expect_s3_class(result$trend_plot, "gg")
  expect_gt(nrow(result$data), 0)
})

test_that("plot_weekly_cases() works with weeks_back as vector", {
  df <- data.frame(
    datum = as.Date("2022-01-01") + 0:210,
    neue_faelle = rpois(211, lambda = 20)
  )

  result <- plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = c(10, 20))

  expect_type(result, "list")
  expect_gt(nrow(result$data), 0)
})

test_that("plot_weekly_cases() throws error for invalid weeks_back", {
  df <- data.frame(
    datum = as.Date("2022-01-01") + 0:30,
    neue_faelle = rpois(31, lambda = 10)
  )

  expect_error(plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = c(50, 60)))
  expect_error(plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = c(5, 1)))
  expect_error(plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = c(1, 2, 3)))
})

test_that("plot_weekly_cases() handles different aggregation functions", {
  df <- data.frame(
    datum = as.Date("2022-01-01") + 0:100,
    neue_faelle = rnorm(101, mean = 20, sd = 5)
  )

  result_mean <- plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = 10, agg_fun = "mean")
  result_sum  <- plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = 10, agg_fun = "sum")

  expect_s3_class(result_mean$trend_plot, "gg")
  expect_s3_class(result_sum$hist_plot, "gg")
})

test_that("plot_weekly_cases() saves plots when save_plot is TRUE", {
  df <- data.frame(
    datum = as.Date("2022-01-01") + 0:100,
    neue_faelle = rpois(101, lambda = 10)
  )

  tmp_dir <- tempdir()
  result <- plot_weekly_cases(df,
                              value_col = "neue_faelle",
                              weeks_back = 5,
                              save_plot = TRUE,
                              save_path = tmp_dir)

  expected_files <- file.path(tmp_dir, c(
    "plot1_trend_sum_neue_faelle.png",
    "plot2_hist_sum_neue_faelle.png",
    "plot3_boxplot_sum_neue_faelle.png"
  ))

  for (file in expected_files) {
    expect_true(file.exists(file), info = paste("File not found:", file))
  }
})

test_that("plot_weekly_cases() handles plottype options correctly", {
  df <- data.frame(
    datum = as.Date("2022-01-01") + 0:100,
    neue_faelle = rpois(101, lambda = 15)
  )

  result_all <- plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = 5, plottype = 1)
  result_histbox <- plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = 5, plottype = 2)
  result_trendbox <- plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = 5, plottype = 3)

  expect_s3_class(result_all$trend_plot, "gg")
  expect_s3_class(result_histbox$hist_plot, "gg")
  expect_s3_class(result_trendbox$box_plot, "gg")
})
