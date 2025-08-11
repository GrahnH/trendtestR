test_that("handles valid minimal input", {
  df <- data.frame(x = 1:10)
  res <- check_input_validity(
    months = 1:2,
    years = 2023,
    shift_month = "none",
    granularity = "day",
    agg_fun = "sum",
    df = df
  )
  expect_type(res, "list")
  expect_equal(res$months, 1:2)
  expect_equal(res$granularity, "day")
})

test_that("Stops on invalid months", {
  df <- data.frame()
  expect_error(check_input_validity(months = 0, years = 2023, shift_month = "none",
                                    granularity = "day", agg_fun = "sum", df = df),
               "Ganzzahlen zwischen 1 und 12")
  expect_error(check_input_validity(months = numeric(0), years = 2023, shift_month = "none",
                                    granularity = "day", agg_fun = "sum", df = df),
               "darf nicht leer sein")
  expect_error(check_input_validity(months = "Jan", years = 2023, shift_month = "none",
                                    granularity = "day", agg_fun = "sum", df = df),
               "numerischer Vektor")
})

test_that("Stops on invalid years", {
  df <- data.frame()
  expect_error(check_input_validity(months = 1, years = character(0), shift_month = "none",
                                    granularity = "day", agg_fun = "sum", df = df),
               "numerischer Vektor")
  expect_error(check_input_validity(months = 1, years = c(2023, 2022), shift_month = "none",
                                    granularity = "day", agg_fun = "sum", df = df),
               "streng aufsteigend")
})

test_that("Stops if duplicated months with shift_month ≠ none", {
  df <- data.frame()
  expect_error(check_input_validity(months = c(1, 1), years = 2023:2024, shift_month = "mth_to_next",
                                    granularity = "week", agg_fun = "mean", df = df),
               "Doppelte Monate")
})

test_that("Warns on missing group_col", {
  df <- data.frame(a = 1:10)
  expect_warning(check_input_validity(1, 2023, "none", "day", "sum", df, group_col = "b"),
                 "nicht in den Spalten")
})

test_that("Messages on non-factor group_col", {
  df <- data.frame(grp = c("A", "B", "A"))
  expect_message(check_input_validity(1, 2023, "none", "day", "sum", df, group_col = "grp"),
                 "keine Faktoren")
})

test_that("Messages on too many levels in group_col", {
  df <- data.frame(biggrp = factor(paste0("G", 1:12)))
  expect_message(check_input_validity(1, 2023, "none", "day", "sum", df, group_col = "biggrp"),
                 "viele Auspraegungen")
})

test_that("Messages when multiple group_col are used", {
  df <- data.frame(a = factor(rep("x", 10)), b = factor(rep("y", 10)))
  expect_message(check_input_validity(1, 2023, "none", "day", "sum", df, group_col = c("a", "b")),
                 "Mehrere Gruppierungsvariablen")
})
