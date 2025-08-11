test_that("Returns correct structure for numeric data", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 450),
    neue_faelle = rnorm(450, mean = 20, sd = 3)
  )

  res_day <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024,2025), months = 1:2, granularity = "day",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )

  res_week <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024,2025), months = 1:2, granularity = "week",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )

  out <- compare_distribution_by_granularity(res_day, res_week, plot = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("factor", "granularity", "shapiro_W", "shapiro_p", "normal") %in% names(out)))
  expect_equal(sort(unique(out$granularity)), c("day", "week"))
})

test_that("Returns correct structure for numeric data with paired test", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2024-03-01"), by = "day", length.out = 450),
    neue_faelle = rnorm(450, mean = 20, sd = 3)
  )

  res_day <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024,2025), months = 3:4, granularity = "day",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )

  res_day$tests <- run_group_tests(res_day$data, value_col = ".value", group_col = "jahr",
                                   alpha = 0.05, effect_size = TRUE,
                                   report_assumptions = TRUE, paired = TRUE)
  res_week <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024,2025), months = 3:4, granularity = "week",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )
  out <- compare_distribution_by_granularity(res_day, res_week, plot = TRUE)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("factor", "granularity", "shapiro_W", "shapiro_p", "normal") %in% names(out)))
  expect_equal(sort(unique(out$granularity)), c("day", "week"))
})

test_that("Returns correct structure for numeric data with more groups",{
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2023-03-01"), by = "day", length.out = 800),
    neue_faelle = rnorm(800, mean = 20, sd = 3)
  )

  res_day <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2023, 2024,2025), months = 3:4, granularity = "day",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )

  res_day$tests <- run_group_tests(res_day$data, value_col = ".value", group_col = "jahr",
                                   alpha = 0.05, effect_size = TRUE,
                                   report_assumptions = TRUE, paired = TRUE)
  res_week <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2023,2024,2025), months = 3:4, granularity = "week",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )
  out <- compare_distribution_by_granularity(res_day, res_week, plot = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("factor", "granularity", "shapiro_W", "shapiro_p", "normal") %in% names(out)))
  expect_equal(sort(unique(out$granularity)), c("day", "week"))
})

test_that("Handles count data gracefully without Shapiro", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 450),
     neue_faelle = rpois(450, lambda = 20)
  )

  res_day <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024,2025), months = 1:2, granularity = "day",
    shift_month = "none", agg_fun = "sum",
    save_plot = FALSE
  )

  res_week <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024,2025), months = 1:2, granularity = "week",
    shift_month = "none", agg_fun = "sum",
    save_plot = FALSE
  )
  attr(res_day$data, "value_data_type") <- "count"
  attr(res_week$data, "value_data_type") <- "count"

  out <- compare_distribution_by_granularity(res_day, res_week, plot = FALSE)
  expect_true(all(out$normal == "nicht anwendbar"))
})


test_that("Throws error when group_col is missing", {
  res_day <- list(data = data.frame(), tests = list(group_col = NULL))
  res_week <- res_day
  expect_error(compare_distribution_by_granularity(res_day, res_week, plot = FALSE),
               "group_col wurde in tests nicht gespeichert")
})


test_that("QQ plot runs silently with correct input", {
   set.seed(999)
   df <- data.frame(
    datum = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 450),
    neue_faelle = rnorm(450, mean = 20, sd = 3)
  )

  res_day <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024, 2025), months = 1:2, granularity = "day",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )

  res_week <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024, 2025), months = 1:2, granularity = "week",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )

  out <- compare_distribution_by_granularity(res_day, res_week, plot = TRUE, save_plot = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("granularity", "shapiro_W") %in% colnames(out)))
})


test_that("Saves QQ plot when save_plot = TRUE", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 450),
    neue_faelle = rnorm(450, mean = 20, sd = 3)
  )

  tmpdir <- tempdir()

  res_day <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024, 2025), months = 1:2, granularity = "day",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )

  res_week <- compare_monthly_cases(
    df = df, datum_col = "datum", value_col = "neue_faelle",
    years = c(2024, 2025), months = 1:2, granularity = "week",
    shift_month = "none", agg_fun = "mean",
    save_plot = FALSE
  )

  compare_distribution_by_granularity(res_day, res_week, plot = TRUE, save_plot = TRUE, save_path = tmpdir)

  saved <- list.files(tmpdir, pattern = "qqplot_by_granularity_.*\\.png", full.names = TRUE)
  expect_true(any(file.exists(saved)))
})
