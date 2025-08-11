test_that("runs correctly with default settings", {
  test_data <- data.frame(
    date = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
    count = rpois(20, lambda = 10)
  )

  result <- explore_poisson_trend(
    data = test_data,
    datum_col = "date",
    value_col = "count",
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_s3_class(result$model, "gam")
  expect_true(!is.null(result$plot))
  expect_gt(result$effective_df, 0)
})

test_that("works with group_col", {
  test_data <- data.frame(
    date = rep(seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 10), 2),
    count = rpois(20, lambda = 10),
    group = rep(c("A", "B"), each = 10)
  )

  result <- explore_poisson_trend(
    data = test_data,
    datum_col = "date",
    value_col = "count",
    group_col = "group",
    verbose = FALSE
  )

  expect_s3_class(result$model, "gam")
  expect_true("A" %in% levels(result$model$model$group))
})

test_that("fails on wrong input", {
  bad_data <- data.frame(
    date = rep("wrong", 10),
    count = letters[1:10]
  )

  expect_error(
    explore_poisson_trend(
      data = bad_data,
      datum_col = "date",
      value_col = "count"
    ),
    regexp = "muss.*Date.*sein"
  )
})

test_that("returns formula if return_formula = TRUE", {
  test_data <- data.frame(
    date = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 10),
    count = rpois(10, lambda = 10)
  )

  result <- explore_poisson_trend(
    data = test_data,
    datum_col = "date",
    value_col = "count",
    return_formula = TRUE
  )

  expect_s3_class(result, "formula")
})

df <- readRDS(testthat::test_path("testdata/sim_df.rds"))
test_that("runs on simulated data", {
  df <- readRDS(testthat::test_path("testdata/sim_df.rds"))
  res <- explore_poisson_trend(data = df,
                               datum_col = "date",
                               value_col = "cases",
                               group_col = "group",
                               family = "auto",
                               verbose = TRUE)
  expect_type(res, "list")
  expect_true("model" %in% names(res))
  expect_s3_class(res$plot, "gg")
})

test_that("Raise error when inputs are invalid", {
  df <- data.frame(
    datum = seq.Date(Sys.Date(), by = "day", length.out = 10),
    value = rpois(10, 5)
  )

  expect_error(explore_poisson_trend(data = df, datum_col = "nope", value_col = "value"))
  expect_error(explore_poisson_trend(data = df, datum_col = "datum", value_col = "nope"))
  expect_error(explore_poisson_trend(data = df, datum_col = "datum", value_col = "value", group_col = "g"))

  df2 <- df
  df2$datum <- as.character(df2$datum)
  expect_error(explore_poisson_trend(df2, datum_col = "datum", value_col = "value"))

  df3 <- df
  df3$value[1] <- -3
  expect_error(explore_poisson_trend(df3, datum_col = "datum", value_col = "value"))
})


test_that("Ignore group_col when only one level and fit model", {
  df <- data.frame(
    datum = seq.Date(Sys.Date(), by = "day", length.out = 20),
    value = rpois(20, 5),
    group = rep("A", 20)
  )

  result <- explore_poisson_trend(df, datum_col = "datum", value_col = "value", group_col = "group")
  expect_s3_class(result$model, "gam")
  expect_match(result$model_family_used, "Poisson")
})

test_that("Raise error when all time values are identical", {
  df <- data.frame(
    datum = rep(Sys.Date(), 10),
    value = rpois(10, 5)
  )

  expect_error(explore_poisson_trend(df, datum_col = "datum", value_col = "value"))
})

test_that("Fit model when family is specified manually", {
  df_p <- data.frame(
    datum = seq.Date(Sys.Date(), by = "day", length.out = 60),
    value = rpois(60, lambda = 5)
  )

  res_p <- explore_poisson_trend(df_p, datum_col = "datum", value_col = "value", family = "poisson")
  expect_match(res_p$model_family_used, "Poisson")
  expect_true(is.numeric(res_p$dispersion_parameter$overdispersion_phi))


  df_nb <- data.frame(
    datum = seq.Date(Sys.Date(), by = "day", length.out = 60),
    value = rnbinom(60, size = 2, mu = 5)
  )

  res_nb <- explore_poisson_trend(df_nb, datum_col = "datum", value_col = "value", family = "negbin")
  expect_match(res_nb$model_family_used, "Negbin")
  expect_true("theta" %in% names(res_nb$dispersion_parameter))
})

test_that("Falls back to NB-GAM when phi > 1.5 and NB has lower AIC", {
  set.seed(123)
  n <- 100
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = n),
    value = rpois(n, lambda = 10) + rbinom(n, 1, 0.4) * rpois(n, lambda = 10)
  )

  result <- explore_poisson_trend(
    data = df,
    datum_col = "datum",
    value_col = "value",
    family = "auto",
    k_spline = 6,
    verbose = TRUE
  )
  result$dispersion_parameter$overdispersion_phi
  expect_true(result$dispersion_parameter$overdispersion_phi > 1.5)
  expect_equal(result$model_family_used, "Negbin GAM (automatisch)")
  expect_type(result$model, "list")
  expect_s3_class(result$model, "gam")
  expect_true(grepl("NB", result$dispersion_parameter$message))
})
test_that("Keeps Poisson GAM if NB-GAM has higher AIC despite phi > 1.5", {
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 20),
    value = c(5,5,6,1,7,5,6,12,4,5,6,4,5,5,6,4,12,5,6,4)
  )

  result <- explore_poisson_trend(
    data = df,
    datum_col = "datum",
    value_col = "value",
    family = "auto",
    k_spline = 6,
    phi = 1,
    verbose = TRUE
  )
  expect_true(result$dispersion_parameter$overdispersion_phi > 1)
  expect_equal(result$model_family_used, "Poisson GAM (automatisch)")
  expect_type(result$model, "list")
  expect_s3_class(result$model, "gam")
  expect_true(grepl("Poisson beibehalten", result$dispersion_parameter$message))
})

