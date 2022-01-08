

test_that("implicit_keys", {
  df1 <- data.frame(id1 = 1:2, val1 = 1:2)
  df2 <- data.frame(id2 = 2:3, val2 = 2:3)
  expect_error(
    power_inner_join(df1, df2, check = check_specs(implicit_keys = "abort")),
    "should be explicit"
  )
})

test_that("column conflict", {
  df1 <- data.frame(id = 1:2, val = 1:2)
  df2 <- data.frame(id = 2:3, val = 3:4)

  expect_error(
    power_inner_join(df1, df2, by = "id", check = check_specs(column_conflict = "abort")),
    "are conflicted"
  )

  expect_message(
    power_inner_join(df1, df2, by = "id", check = check_specs(column_conflict = "inform")),
    "are conflicted"
  )

  expect_error(
    power_inner_join(df1, df2, by = ~ .x$id < .y$id, check = check_specs(column_conflict = "abort")),
    "are conflicted"
  )

  expect_message(
    power_inner_join(df1, df2, by = ~ .x$id < .y$id, check = check_specs(column_conflict = "inform")),
    "are conflicted"
  )
})


test_that("grouped_input", {
  df1 <- data.frame(id = 1:2, val = 1:2) |> dplyr::group_by(id)
  df2 <- data.frame(id = 2:3, val = 3:4) |> dplyr::group_by(id)

  expect_error(
    power_inner_join(df1, df2, by = "id", check = check_specs(grouped_input = "abort")),
    "grouped"
  )

  expect_message(
    power_inner_join(df1, df2, by = "id", check = check_specs(grouped_input = "inform")),
    "grouped"
  )

  expect_error(
    power_inner_join(df1, df2, by = ~ .x$id < .y$id, check = check_specs(grouped_input = "abort")),
    "grouped"
  )

  expect_message(
    power_inner_join(df1, df2, by = ~ .x$id < .y$id, check = check_specs(grouped_input = "inform")),
    "grouped"
  )
})


test_that("na_keys", {
  x <- data.frame(key = c(1, NA), x = 1:2)
  y <- data.frame(key = 1, y = 1)

  expect_error(
    power_inner_join(x, y, by = "key", check = check_specs(na_keys = "abort")),
    "left"
  )
  expect_error(
    power_inner_join(y, x, by = "key", check = check_specs(na_keys = "abort")),
    "right"
  )
})


