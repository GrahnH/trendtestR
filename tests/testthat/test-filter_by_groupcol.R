
test_that("Basic filtering without keep_levels and wide format", {
  df <- data.frame(
    datum = as.Date("2024-01-01") + 0:3,
    gruppe = rep(c("A", "B"), each = 2),
    neue_faelle = c(1, 2, 3, 4)
  )
  out <- filter_by_groupcol(df, "gruppe", "neue_faelle", "datum")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4)
})

test_that("Filters only specified keep_levels", {
  df <- data.frame(
    datum = as.Date("2024-01-01") + 0:3,
    gruppe = rep(c("A", "B"), each = 2),
    neue_faelle = 1:4
  )
  out <- filter_by_groupcol(df, "gruppe", "neue_faelle", "datum", keep_levels = "A")
  expect_true(all(out$gruppe == "A"))
})

test_that("Warns when keep_levels includes invalid group", {
  df <- data.frame(
    datum = as.Date("2024-01-01") + 0:3,
    gruppe = rep("A", 4),
    neue_faelle = 1:4
  )
  expect_warning(
    out <- filter_by_groupcol(df, "gruppe", "neue_faelle", "datum", keep_levels = c("A", "B")),
    regexp = "nicht vorhanden"
  )
  expect_true(all(out$gruppe == "A"))
})

test_that("Wide format conversion creates new columns", {
  df <- data.frame(
    datum = rep(as.Date("2024-01-01") + 0:2, each = 2),
    gruppe = rep(c("A", "B"), times = 3),
    neue_faelle = 1:6
  )
  out <- filter_by_groupcol(df, "gruppe", "neue_faelle", "datum", to_wide = TRUE)
  expect_true(all(c("neue_faelle_A", "neue_faelle_B") %in% names(out)))
})

test_that("keep_other_cols = TRUE preserves extra columns", {
  df <- data.frame(
    datum = rep(as.Date("2024-01-01") + 0:1, each = 2),
    gruppe = rep(c("A", "B"), 2),
    neue_faelle = 1:4,
    region = "Berlin"
  )
  out <- filter_by_groupcol(df, "gruppe", "neue_faelle", "datum", keep_other_cols = TRUE)
  expect_true("region" %in% names(out))
})

test_that("Throws error for missing columns", {
  df <- data.frame(datum = as.Date("2024-01-01"), val = 1)
  expect_error(filter_by_groupcol(df, "gruppe", "val", "datum"))
})

test_that("Throws error when df is not a data.frame", {
  expect_error(filter_by_groupcol("not a df", "gruppe", "val", "datum"))
})

test_that("Throws error on invalid logical arguments", {
  df <- data.frame(
    datum = Sys.Date(),
    gruppe = "A",
    neue_faelle = 1
  )
  expect_error(filter_by_groupcol(df, "gruppe", "neue_faelle", "datum", to_wide = "yes"))
  expect_error(filter_by_groupcol(df, "gruppe", "neue_faelle", "datum", keep_other_cols = c(TRUE, FALSE)))
})
