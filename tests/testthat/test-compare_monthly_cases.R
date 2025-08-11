test_that("compare_monthly_cases handles cross-year logic, group_col, and plot saving", {
  df <- data.frame(
    datum = seq.Date(from = as.Date("2024-12-25"), to = as.Date("2025-01-10"), by = "day"),
    neue_faelle = sample(10:50, size = 17, replace = TRUE),
    region = "Berlin"
  )

  tmpdir <- tempdir()

  result <- compare_monthly_cases(
    df = df,
    datum_col = "datum",
    value_col = "neue_faelle",
    group_col = "region",
    years = c(2024, 2025),
    months = c(12, 1),
    granularity = "day",
    agg_fun = "sum",
    shift_month = "mth_to_next",
    save_plot = TRUE,
    save_path = tmpdir
  )

  expect_type(result, "list")
  expect_s3_class(result$trend_plot, "gg")
  expect_s3_class(result$monthly_trend_plot, "gg")
  expect_s3_class(result$box_plot, "gg")
  expect_true("data" %in% names(result))
  expect_true("tests" %in% names(result))

  expect_true("region" %in% colnames(result$data))
  expect_equal(nlevels(result$data$region), 1)
  expect_equal(unique(result$data$jahr), factor("2025"))

  today <- Sys.Date()
  expect_true(file.exists(file.path(tmpdir, paste0("comepareMtrend_sumneue_faelle_", today, ".png"))))
  expect_true(file.exists(file.path(tmpdir, paste0("comepareMtrend_monthly_sumneue_faelle_", today, ".png"))))
  expect_true(file.exists(file.path(tmpdir, paste0("comepareMbox_sumneue_faelle_", today, ".png"))))
})

test_that(" handles 2-level group_col, cross-year shift (next), and zero values", {
  today <- Sys.Date()
  tmpdir <- tempdir()

  df <- data.frame(
    datum = seq.Date(as.Date("2024-12-28"), as.Date("2025-01-05"), by = "day"),
    neue_faelle = c(0, 12, 18, 0, 25, 30, 15, 0, 10),
    region = rep(c("Berlin", "Bremen"), length.out = 9)
  )

  result <- compare_monthly_cases(
    df = df,
    datum_col = "datum",
    value_col = "neue_faelle",
    group_col = "region",
    years = 2025,
    months = c(12, 1),
    granularity = "day",
    agg_fun = "sum",
    shift_month = "mth_to_next",
    save_plot = TRUE,
    save_path = tmpdir
  )

  expect_type(result, "list")
  expect_s3_class(result$box_plot, "gg")
  expect_equal(nlevels(result$data$region), 2)
  expect_true(all(result$data$jahr == "2025"))
  expect_true(any(result$data$neue_faelle == 0))

  expect_true(file.exists(file.path(tmpdir, paste0("comepareMtrend_sumneue_faelle_", today, ".png"))))
  expect_true(file.exists(file.path(tmpdir, paste0("comepareMtrend_monthly_sumneue_faelle_", today, ".png"))))
  expect_true(file.exists(file.path(tmpdir, paste0("comepareMbox_sumneue_faelle_", today, ".png"))))
})

test_that("handles 3-level group_col, cross-year shift (prev), and negative values", {
  df <- data.frame(
    datum = seq.Date(as.Date("2025-01-01"), as.Date("2025-01-12"), by = "day"),
    neue_faelle = c(-3, 5, 10, 0, 7, 9, -1, 4, 6, 8, 0, 2),
    zone = rep(c("East", "West", "North"), length.out = 12)
  )

  expect_warning(
    result <- compare_monthly_cases(
      df = df,
      datum_col = "datum",
      value_col = "neue_faelle",
      group_col = "zone",
      years = 2024,
      months = c(12, 1),
      granularity = "day",
      agg_fun = "mean",
      shift_month = "mth_to_prev",
      save_plot = FALSE
    ),
    "Boxplot.*nicht-positive Werte"
  )

  expect_type(result, "list")
  expect_s3_class(result$trend_plot, "gg")
  expect_equal(nlevels(result$data$zone), 3)
  expect_equal(unique(result$data$jahr), factor("2024"))
  expect_true(any(result$data$neue_faelle <= 0))
})
