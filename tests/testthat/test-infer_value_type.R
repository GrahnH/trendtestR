
test_that("Detects binary data", {
  x <- c(0, 1, 0, 1, 1)
  out <- infer_value_type(x)
  expect_equal(out$type, "binary")
})

test_that("Detects proportion data", {
  x <- c(0.1, 0.3, 0.6, 0.9)
  out <- infer_value_type(x)
  expect_equal(out$type, "proportion")
})

test_that("Detects count data with high values", {
  x <- c(0, 1, 2, 5, 100)
  out <- infer_value_type(x)
  expect_equal(out$type, "count")
})

test_that("Detects discrete data", {
  x <- c(1, 2, 3, 4, 5)
  out <- infer_value_type(x)
  expect_equal(out$type, "discrete")
})

test_that("Detects continuous data", {
  x <- rnorm(100)
  out <- infer_value_type(x)
  expect_equal(out$type, "continuous")
})

test_that("Handles too few values", {
  x <- c(1, 2)
  out <- infer_value_type(x, verbose = FALSE)
  expect_equal(out$type, "continuous")
})

test_that("Handles negative values correctly", {
  x <- c(-1, -2, -3, -4)
  out <- infer_value_type(x)
  expect_equal(out$type, "continuous")
})

test_that("Warns about large values", {
  x <- c(1e9, 1e9, 1e9, 1e9)
  expect_warning(infer_value_type(x, verbose = TRUE), "Extremwerte erkannt")
})

test_that("Handles NA values properly", {
  x <- c(0, 1, NA, 1, NA)
  out <- infer_value_type(x)
  expect_equal(out$type, "binary")
})

test_that("Computes skewness if needed", {
  x <- c(1.1, 2.5, 3.3, 100.1)
  out <- infer_value_type(x)
  expect_equal(out$type, "continuous")
  expect_true(out$features %in% c("right-skewed", "left-skewed", "symmetric"))
})
