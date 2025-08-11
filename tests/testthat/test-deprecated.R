test_that("explore_poisson_trend_Legacy fits basic Poisson model", {
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 100),
    neue_faelle = rpois(100, lambda = 10)
  )

  expect_warning(
    out <- explore_poisson_trend_Legacy(
      data = df,
      datum_col = "datum",
      value_col = "neue_faelle"
    ),
    "veraltet"
  )

  expect_type(out, "list")
  expect_s3_class(out$model, "glm")
  expect_true("formula" %in% names(out))
  expect_true("dispersion_parameter" %in% names(out))
})

test_that("explore_poisson_trend_Legacy returns formula when requested", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
    neue_faelle = rpois(30, lambda = 5)
  )

  fml <- explore_poisson_trend_Legacy(
    data = df,
    datum_col = "datum",
    value_col = "neue_faelle",
    return_formula = TRUE
  )

  expect_s3_class(fml, "formula")
})

test_that("explore_poisson_trend_Legacy supports group_col interaction", {
   set.seed(999)
   df <- data.frame(
    datum = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 40), 2),
    neue_faelle = c(rpois(40, 5), rpois(40, 15)),
    gruppe = rep(c("A", "B"), each = 40)
  )

  res <- suppressWarnings(explore_poisson_trend_Legacy(
    data = df,
    datum_col = "datum",
    value_col = "neue_faelle",
    group_col = "gruppe"
  ))

  expect_s3_class(res$model, "glm")
  expect_match(res$model_family_used, "Poisson|Negative Binomial")
})

test_that("explore_poisson_trend_Legacy chooses NB model if overdispersion", {
  set.seed(123)
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 500),
    neue_faelle = abs(round(rnorm(500, mean = 10, sd = 10)))
  )

  res <- suppressWarnings(explore_poisson_trend_Legacy(
    data = df,
    datum_col = "datum",
    value_col = "neue_faelle"
  ))

  expect_true(res$dispersion_parameter > 1.2)
  expect_equal(res$model_family_used, "Negative Binomial")
})

test_that("explore_poisson_trend_Legacy fails on invalid input types", {
  df_invalid <- data.frame(
    datum = rep("2023-01-01", 10),
    neue_faelle = 1:10
  )
  expect_error(explore_poisson_trend_Legacy(df_invalid, "datum", "neue_faelle"), "vom Typ")

  df_neg <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 10),
    neue_faelle = c(-1, rep(3, 9))
  )
  expect_error(explore_poisson_trend_Legacy(df_neg, "datum", "neue_faelle"), "darf keine negativen Werte enthalten")
})

test_that("explore_poisson_trend_Legacy stops on insufficient unique values", {
  df_const <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 15),
    neue_faelle = rep(4, 15)
  )
  expect_error(explore_poisson_trend_Legacy(df_const, "datum", "neue_faelle"), "weniger als zwei unterschiedliche Werte")
})

test_that("explore_poisson_trend_Legacy warns on single-level group", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 20),
    neue_faelle = rpois(20, 8),
    gruppe = rep("A", 20)
  )
  expect_warning(
    res <- explore_poisson_trend_Legacy(df, "datum", "neue_faelle", group_col = "gruppe"),
    "enthaelt weniger als zwei Gruppen"
  )
  expect_null(res$aic_comparison)
})
test_that("explore_poisson_trend_Legacy uses Poisson model when no overdispersion", {

  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 100),
    neue_faelle = rpois(100, lambda = 5)
  )

  res <- suppressWarnings(explore_poisson_trend_Legacy(df, "datum", "neue_faelle"))
  expect_lte(res$dispersion_parameter, 1.5)
  expect_equal(res$model_family_used, "Poisson")
})

test_that("explore_poisson_trend_Legacy chooses NB model when overdispersed and AIC is lower", {
  set.seed(123)
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 500),
    neue_faelle = abs(round(rnorm(500, mean = 10, sd = 20)))
  )

  res <- suppressWarnings(explore_poisson_trend_Legacy(df, "datum", "neue_faelle"))
  expect_gt(res$dispersion_parameter, 1.5)
  expect_equal(res$model_family_used, "Negative Binomial")
})

test_that("explore_poisson_trend_Legacy uses Poisson when no overdispersion", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 100),
    neue_faelle = rpois(100, lambda = 5)
  )

  res <- suppressWarnings(explore_poisson_trend_Legacy(df, "datum", "neue_faelle"))

  expect_equal(res$model_family_used, "Poisson")
  expect_match(res$model_selection_info, "Poisson-Modell wird verwendet")
})


test_that("explore_poisson_trend_Legacy uses Poisson model when family = 'poisson'", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 100),
    neue_faelle = rnbinom(100, mu = 10, size = 0.3)
  )

  res <- suppressWarnings(explore_poisson_trend_Legacy(
    df, "datum", "neue_faelle", family = "poisson"
  ))
  expect_equal(res$model_family_used, "Poisson")
})

test_that("explore_poisson_trend_Legacy uses NB model when family = 'negbin'", {
   set.seed(999)
   df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 100),
    neue_faelle = rnbinom(100, mu = 10, size = 0.5)
  )

  res <- suppressWarnings(explore_poisson_trend_Legacy(
    df, "datum", "neue_faelle", family = "negbin"
  ))
  expect_equal(res$model_family_used, "Negative Binomial")
})

test_that("explore_poisson_trend_Legacy throws error when values are constant", {
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 50),
    neue_faelle = rep(3, 50)
  )

  expect_error(explore_poisson_trend_Legacy(df, "datum", "neue_faelle"))
})

test_that("explore_poisson_trend_Legacy warns when group_col has only one level", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 20),
    neue_faelle = rpois(20, 8),
    gruppe = rep("A", 20)
  )

  expect_warning(
    res <- explore_poisson_trend_Legacy(df, "datum", "neue_faelle", group_col = "gruppe"),
    "enthaelt weniger als zwei Gruppen"
  )
  expect_null(res$aic_comparison)
})

test_that("explore_poisson_trend_Legacy returns formula when return_formula = TRUE", {
  set.seed(999)
  df <- data.frame(
    datum = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
    neue_faelle = rpois(30, lambda = 5)
  )

  fml <- suppressWarnings(explore_poisson_trend_Legacy(
    data = df,
    datum_col = "datum",
    value_col = "neue_faelle",
    return_formula = TRUE
  ))

  expect_s3_class(fml, "formula")
})
