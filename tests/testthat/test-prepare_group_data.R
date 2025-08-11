test_that("Returns expected structure on valid input", {
  df <- data.frame(
    jahr = rep(c("2020", "2021", "2022"), each = 5),
    .value = c(1:5, 6:10, 11:15)
  )

  result <- prepare_group_data(df)

  expect_type(result, "list")
  expect_named(result, c("df", "vals", "sample_sizes", "n_groups", "group_names"))
  expect_equal(result$n_groups, 3)
  expect_equal(result$sample_sizes, c("2020" = 5, "2021" = 5, "2022" = 5))
  expect_equal(names(result$vals), c("2020", "2021", "2022"))
  expect_true(is.factor(result$df$jahr))
})


test_that("Drops groups with only NA and issues message", {
  df <- data.frame(
    jahr = factor(rep(c("A", "B", "C"), each = 4)),
    .value = c(1:4, NA, NA, NA, NA, 9:12)
  )

  expect_message(
    result <- prepare_group_data(df),
    "Hinweis: Folgende Gruppen wurden ausgeschlossen"
  )
  expect_equal(result$n_groups, 2)
  expect_false("B" %in% result$group_names)
})


test_that("Handles one valid group", {
  df <- data.frame(
    jahr = rep("X", 5),
    .value = 1:5
  )

  result <- prepare_group_data(df)
  expect_equal(result$n_groups, 1)
  expect_equal(unname(result$sample_sizes["X"]), 5)
  expect_true(is.factor(result$df$jahr))
})


test_that("Throws error for missing group_col", {
  df <- data.frame(
    not_a_group = rep(c("a", "b"), 5),
    .value = rnorm(10)
  )

  expect_error(
    prepare_group_data(df, group_col = "missing_col"),
    "Die Spalte 'missing_col' wurde im Data Frame 'df' nicht gefunden"
  )
})


test_that("Handles all NA values gracefully", {
  df <- data.frame(
    jahr = rep(c("A", "B"), each = 5),
    .value = NA_real_
  )

  result <- prepare_group_data(df)
  expect_equal(result$n_groups, 0)
  expect_length(result$vals, 0)
  expect_equal(length(result$sample_sizes), 0)
})


test_that("Works with numeric or character group_col", {
  df1 <- data.frame(group = rep(1:3, each = 3), .value = 1:9)
  df2 <- data.frame(group = rep(c("x", "y", "z"), each = 3), .value = 10:18)

  res1 <- prepare_group_data(df1, group_col = "group")
  res2 <- prepare_group_data(df2, group_col = "group")

  expect_equal(res1$n_groups, 3)
  expect_equal(res2$n_groups, 3)
  expect_type(res1$group_names, "character")
  expect_type(res2$group_names, "character")
})
test_that("Throws error when value_col is not numeric", {
  df <- data.frame(
    jahr = c("2020", "2021"),
    .value = c("a", "b")
  )

  expect_error(
    prepare_group_data(df),
    "muss numerisch sein"
  )
})


test_that("Throws error when df is not a data.frame", {
  expect_error(
    prepare_group_data(list(a = 1:5, b = letters[1:5])),
    "muss ein Data Frame sein"
  )
})

