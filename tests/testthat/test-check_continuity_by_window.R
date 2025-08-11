test_that("Fails if date_vec is empty", {
  expect_error(
    check_continuity_by_window(
      date_vec = as.Date(character()),
      years = 2021, months = 1:3, window_unit = "day"
    ),
    "enthaelt keine gueltigen Daten"
  )
})

test_that("Fails if date_vec does not cover the window", {
  vec <- as.Date("2021-01-15")
  expect_error(
    check_continuity_by_window(
      date_vec = vec,
      years = 2022, months = 1:2, window_unit = "day"
    ),
    "liegt ausserhalb der Datenabdeckung"
  )
})

test_that("Detects full continuity (daily)", {
  vec <- seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = "day")
  res <- check_continuity_by_window(vec, years = 2021, months = 1, window_unit = "day")
  expect_true(res$continuous)
  expect_null(res$gaps)
})

test_that("Detects missing dates (daily)", {
  vec <- seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = "day")[-10]
  res <- check_continuity_by_window(vec, years = 2021, months = 1, window_unit = "day")
  expect_false(res$continuous)
  expect_s3_class(res$gaps, "data.frame")
  expect_true(as.Date("2021-01-10") %in% res$gaps$missing)
})

test_that("Allows leading gap if specified", {
  Sys.setenv(TZ = "UTC")
  vec <- seq(as.Date("2020-12-30"), as.Date("2021-01-31"), by = "day")
  vec <- vec[-(2:5)]
  res <- check_continuity_by_window(vec, years = 2021, months = 1, start_date = "2021-01-02", window_unit = "day", allow_leading_gap = TRUE)
  res$gaps
  res$continuous
  res
  expect_true(res$continuous)
})

test_that("Detects weekly continuity with isoweek = TRUE", {
  vec <- seq(as.Date("2020-12-28"), as.Date("2021-01-31"), by = "7 days")
  res <- check_continuity_by_window(vec, years = 2021, months = 1, window_unit = "week", use_isoweek = TRUE)
  expect_true(res$continuous)
})

test_that("Detects missing weekly dates", {
  vec <- seq(as.Date("2020-12-28"), as.Date("2021-01-25"), by = "7 days")
  vec <- vec[-2]
  res <- check_continuity_by_window(vec, years = 2021, months = 1, window_unit = "week", use_isoweek = TRUE)
  expect_false(res$continuous)
})


test_that("Detects monthly continuity", {
  vec <- seq(as.Date("2020-12-01"), as.Date("2021-12-31"), by = "day")
  res <- check_continuity_by_window(vec, years = 2021, months = 1:12, window_unit = "month")
  expect_true(res$continuous)
})


test_that("Warns if no data in window", {
  vec <- seq(as.Date("2021-01-01"), as.Date("2021-02-28"), by = "day")
  vec <- as.Date("2020-12-31")
  expect_error(
    check_continuity_by_window(vec, years = 2021, months = 1:2, window_unit = "day"),
    regexp = "liegt ausserhalb der Datenabdeckung"
  )
})


test_that("Warns if expected dates exceed available data", {
  vec_full <- seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = "day")
  vec <- vec_full[1:24]
  vec <- c(vec, as.Date("2021-02-01"))
  res <- check_continuity_by_window(vec, years = 2021, months = 1, window_unit = "day")
  res
  expect_false(res$continuous)
})

test_that("Handles empty date vector within specified window", {
  library(lubridate)
  date_vec <- seq(as.Date("2020-01-01"), as.Date("2020-04-01"), by = "day")
  date_vec <- date_vec[month(date_vec) != 3]
  date_vec
  res <- check_continuity_by_window(
    date_vec = date_vec,
    years = 2020,
    months = 3,
    window_unit = "week"
  )
  expect_false(res$continuous)
  expect_s3_class(res$gaps, "data.frame")
  expect_equal(nrow(res$gaps), 5)

  expect_equal(length(res$datum), 0)
  expect_warning(
    check_continuity_by_window(date_vec, years = 2020, months = 3),
    "Keine Daten im angegebenen Zeitfenster vorhanden."
  )
})

test_that("Stops when years parameter is NULL", {
  expect_error(
    check_continuity_by_window(
      date_vec = test_dates,
      years = NULL,
      months = 1:3,
      window_unit = "day"
    ),
    regexp = "Jahre und Monate muessen angegeben werden\\."
  )
})

test_that("Stops when months parameter is empty", {
  expect_error(
    check_continuity_by_window(
      date_vec = test_dates,
      years = 2021,
      months = numeric(0),
      window_unit = "day"
    ),
    regexp = "Jahre und Monate muessen angegeben werden\\."
  )
})
test_that("Stops when months contains values greater than 12", {
  expect_error(
    check_continuity_by_window(
      date_vec = test_dates,
      years = 2021,
      months = c(11, 12, 13),
      window_unit = "day"
    ),
    regexp = "Monate muessen zwischen 1 und 12 liegen\\."
  )
})
test_that("Stops when step is negative", {
  expect_error(
    check_continuity_by_window(
      date_vec = test_dates,
      years = 2021,
      months = 1:3,
      window_unit = "day",
      step = -1
    ),
    regexp = "Schrittweite muss groesser als 0 sein\\."
  )
})
