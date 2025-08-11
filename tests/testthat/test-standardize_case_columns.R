test_that("Converts ymd date and numeric values correctly", {
  df <- data.frame(
    datum = c("2021-01-01", "2021-01-02"),
    neue_faelle = c("12", "15")
  )

  res <- standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle", verbose = FALSE)

  expect_s3_class(res$datum, "Date")
  expect_equal(res$.value, c(12, 15))
  expect_type(res$.value, "double")
})

test_that("Fails if value_col not in df", {
  df <- data.frame(datum = c("2020-01-01"), werte = c(1))
  expect_error(standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle"),
               "Spalte 'neue_faelle' fehlt")
})

test_that("Fails if datum_col not in df", {
  df <- data.frame(tag = c("2020-01-01"), neue_faelle = c(3))
  expect_error(standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle"),
               "Spalte 'datum' fehlt")
})

test_that("Fails if date conversion fails completely", {
  df <- data.frame(datum = c("abc", "def"), neue_faelle = c(3, 4))
  expect_error(standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle", verbose = FALSE),
               "konnte nicht erfolgreich in ein Datumsformat")
})

test_that("Warns if partial date conversion fails", {
  df <- data.frame(
    datum = c("2021-01-01", "not_a_date", "2021-01-03"),
    neue_faelle = c(3, 4, 5)
  )
  expect_warning(
    standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle"),
    regexp = "konvertiert"
  )
})


test_that("Skips date conversion if already Date or POSIXct", {
  df <- data.frame(datum = as.Date(c("2021-01-01", "2021-01-02")), neue_faelle = c(1, 2))
  res <- standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle", verbose = FALSE)
  expect_s3_class(res$datum, "Date")
})

test_that("Parses character value_col with commas", {
  df <- data.frame(datum = "2021-01-01", neue_faelle = "1,5")
  res <- standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle", verbose = FALSE)
  expect_equal(res$.value, 1.5)
})

test_that("Handles existing monat column as ordered factor", {
  df <- data.frame(
    datum = c("2021-01-01", "2021-02-01"),
    neue_faelle = c(1, 2),
    monat = c("Jan", "Feb")
  )
  res <- standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle", verbose = FALSE)
  expect_true(is.ordered(res$monat))
  expect_equal(levels(res$monat)[1], "Jan")
})

test_that("Returns .value column equal to value_col", {
  df <- data.frame(datum = "2021-01-01", neue_faelle = 42)
  res <- standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle", verbose = FALSE)
  expect_equal(res$.value, 42)
})

test_that("Skips type inference if value_data_type attribute exists", {
  df <- data.frame(datum = "2021-01-01", neue_faelle = 3)
  attr(df, "value_data_type") <- "custom"
  attr(df, "value_data_feature") <- "abc"

  res <- standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle", verbose = FALSE)
  expect_equal(attr(res, "value_data_type"), "custom")
  expect_equal(attr(res, "value_data_feature"), "abc")
})

test_that("Infers type using infer_value_type if no attribute exists", {
  called <- FALSE
  fake_infer <- function(x, verbose = TRUE) {
    called <<- TRUE
    list(type = "count", features = "skewed")
  }

  df <- data.frame(datum = "2021-01-01", neue_faelle = 4)

  mockr::local_mock(infer_value_type = fake_infer)
  res <- standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle", verbose = FALSE)

  expect_true(called)
  expect_equal(attr(res, "value_data_type"), "count")
  expect_equal(attr(res, "value_data_feature"), "skewed")
})

test_that("Falls back to 'continuous' if type inference fails", {
  fake_broken <- function(x, verbose = TRUE) stop("boom")

  df <- data.frame(datum = "2021-01-01", neue_faelle = 5)

  mockr::local_mock(infer_value_type = fake_broken)
  res <- standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle", verbose = FALSE)

  expect_equal(attr(res, "value_data_type"), "continuous")
})
