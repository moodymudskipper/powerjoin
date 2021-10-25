df1 <- data.frame(id1 = 1:2, val1 = 1:2)
df2 <- data.frame(id2 = 2:3, val2 = 2:3)

test_that("implicit_keys", {
  expect_error(
    power_inner_join(df1, df2, check = check_specs(implicit_keys = "abort")),
    "should be explicit"
  )
})

df1 <- data.frame(id = 1:2, val = 1:2)
df2 <- data.frame(id = 2:3, val = 3:4)

test_that("column conflict", {
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
