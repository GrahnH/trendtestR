test_that("explore_continuous_trend() basic Gaussian works", {
  df <- readRDS(testthat::test_path("testdata/sim_cont_df.rds"))
  expect_silent({
    res <- explore_continuous_trend(
      data = df,
      datum_col = "date",
      value_col = "y",
      df_spline = 1,
      family = "gaussian"
    )
  })
  expect_type(res, "list")
  expect_s3_class(res$model, "glm")
  expect_match(res$model_family_used, "Gaussian")
  expect_false(is.null(res$plot))
})

test_that("explore_continuous_trend() automatic family selection works", {
  df <- readRDS(testthat::test_path("testdata/sim_cont_df.rds"))
  expect_silent({
    res <- explore_continuous_trend(
      data = df,
      datum_col = "date",
      value_col = "y",
      df_spline = 2,
      family = "auto"
    )
  })
  expect_type(res, "list")
  expect_s3_class(res$model, "glm")
  expect_true(res$model_family_used %in% c("Gaussian GLM (automatisch)", "Gamma GLM (automatisch)"))
  expect_true(is.null(res$aic_comparison) || inherits(res$aic_comparison, "data.frame"))
})

test_that("explore_continuous_trend() with group_col fits and plots", {
  df <- readRDS(testthat::test_path("testdata/sim_cont_df.rds"))
  expect_silent({
    res <- explore_continuous_trend(
      data = df,
      datum_col = "date",
      value_col = "y",
      group_col = "group",
      df_spline = 2,
      family = "auto"
    )
  })
  expect_s3_class(res$model, "glm")
  expect_true(any(grepl("group", names(coef(res$model)))))
  expect_false(is.null(res$plot))
})

test_that("explore_continuous_trend() returns formula if requested", {
  df <- readRDS(testthat::test_path("testdata/sim_cont_df.rds"))
  fml <- explore_continuous_trend(
    data = df,
    datum_col = "date",
    value_col = "y",
    group_col = "group",
    df_spline = 2,
    return_formula = TRUE
  )
  expect_s3_class(fml, "formula")
})

