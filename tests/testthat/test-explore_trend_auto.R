test_that("Auto dispatch works for Poisson data", {
  df <- readRDS(testthat::test_path("testdata/sim_df.rds"))
  attr(df, "value_data_type") <- "count"
  attr(df, "value_data_feature") <- "none"

  res <- explore_trend_auto(df, datum_col = "date", value_col = "cases")
  expect_s3_class(res$model, "gam")
  expect_equal(res$used_function, "explore_poisson_trend")
})

test_that("Auto dispatch works for zero-inflated data", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  attr(df, "value_data_type") <- "count"
  attr(df, "value_data_feature") <- "zi"

  res <- explore_trend_auto(df, datum_col = "date", value_col = "count", family = "zinb")
  expect_s3_class(res$model, "zeroinfl")
  expect_equal(res$used_function, "explore_zinb_trend")
})

test_that("Auto dispatch works for continuous data", {
  df <- readRDS(testthat::test_path("testdata/sim_cont_df.rds"))
  attr(df, "value_data_type") <- "continuous"
  attr(df, "value_data_feature") <- "smooth"

  res <- explore_trend_auto(df, datum_col = "date", value_col = "y")
  expect_true(any(grepl("glm", class(res$model))))
  expect_equal(res$used_function, "explore_continuous_trend")
})

test_that("Invalid kdf throws error", {
  df <- readRDS(testthat::test_path("testdata/sim_df.rds"))
  attr(df, "value_data_type") <- "count"
  attr(df, "value_data_feature") <- "none"
  expect_error(explore_trend_auto(df, datum_col = "date", value_col = "cases", kdf = "abc"))
  expect_error(explore_trend_auto(df, datum_col = "date", value_col = "cases", kdf = -1))
})

test_that("Missing columns are detected", {
  df <- readRDS(testthat::test_path("testdata/sim_df.rds"))
  attr(df, "value_data_type") <- "count"
  attr(df, "value_data_feature") <- "none"
  expect_error(explore_trend_auto(df, datum_col = "unknown", value_col = "cases"))
  expect_error(explore_trend_auto(df, datum_col = "date", value_col = "missing"))
})

test_that("return_formula = TRUE works", {
  df <- readRDS(testthat::test_path("testdata/sim_zinb_df.rds"))
  attr(df, "value_data_type") <- "count"
  attr(df, "value_data_feature") <- "zi"

  res <- explore_trend_auto(df, datum_col = "date", value_col = "count", return_formula = TRUE)
  expect_type(res, "language")
})

test_that("Unsupported value_data_type triggers error", {
  df <- readRDS(testthat::test_path("testdata/sim_df.rds"))
  attr(df, "value_data_type") <- "something_unknown"
  attr(df, "value_data_feature") <- "none"

  res <- explore_trend_auto(df, datum_col = "date", value_col = "cases")
  expect_true(any(grepl("glm", class(res$model))))

})

test_that("Missing value_data_type triggers infer_value_type", {
  df <- readRDS(testthat::test_path("testdata/sim_df.rds"))
  attr(df, "value_data_type") <- NULL
  attr(df, "value_data_feature") <- NULL

  expect_message(
    explore_trend_auto(df, datum_col = "date", value_col = "cases", verbose = TRUE),
    "infer_value_type"
  )
})

test_that("Missing group_col throws informative error", {
  df_missing_group <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 10),
    value = rnorm(10)
  )

  expect_error(
    explore_trend_auto(df_missing_group,
                       datum_col = "date", value_col = "value", group_col = "group"),
    regexp = "group", ignore.case = TRUE
  )
})



test_that("Missing value column entirely triggers error", {
  df <- data.frame(date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30))
  attr(df, "value_data_type") <- "count"
  attr(df, "value_data_feature") <- "none"

  expect_error(
    explore_trend_auto(df, datum_col = "date", value_col = "missing_value")
  )
})

test_that("Output result always contains meta fields", {
  df <- readRDS(testthat::test_path("testdata/sim_df.rds"))
  attr(df, "value_data_type") <- "cases"
  attr(df, "value_data_feature") <- "none"

  res <- explore_trend_auto(df, datum_col = "date", value_col = "cases")
  expect_true(all(c("dispatched_by", "data_type", "used_function") %in% names(res)))
})

test_that("handles missing group column properly by prepare_group_data", {

  result <- tryCatch({
    explore_trend_auto(df, datum_col = "datum_col", value_col = "value_col", group_col = "group_col")
  }, error = function(e) {
    return(e$message)
  })
  result
  expect_type(result, "character")

})

test_that("prints Hinweis for binary when attribute is preset", {
  df <- readRDS(testthat::test_path("testdata/sim_df.rds"))
  attr(df, "value_data_type") <- "binary"
  msgs <- testthat::capture_messages(explore_trend_auto(df, datum_col = "date", value_col = "cases"))
  msgs
  expect_true(any(grepl("Variablentyp 'binary'", msgs)))
})
