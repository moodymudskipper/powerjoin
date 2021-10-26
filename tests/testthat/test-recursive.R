df1 <- data.frame(id = 1:2, val1 = 1:2)
df2 <- data.frame(id = 1:2, val2 = 3:4)
df3 <- data.frame(id = 1:2, val3 = 5:6)
out <- data.frame(id = 1:2, val1 = 1:2, val2 = 3:4, val3 = 5:6)

test_that("recursive joins work", {

  expect_equal(
    power_left_join(df1, list(df2, df3), by = "id"),
    out
  )

  expect_equal(
    power_left_join(list(df1, df2, df3), by = "id"),
    out
  )

  expect_equal(
    power_left_join(list(df1, df2), df3, by = "id"),
    out
  )

  expect_equal(
    power_right_join(df1, list(df2, df3), by = "id"),
    out
  )

  expect_equal(
    power_right_join(list(df1, df2, df3), by = "id"),
    out
  )

  expect_equal(
    power_right_join(list(df1, df2), df3, by = "id"),
    out
  )

  expect_equal(
    power_inner_join(df1, list(df2, df3), by = "id"),
    out
  )

  expect_equal(
    power_inner_join(list(df1, df2, df3), by = "id"),
    out
  )

  expect_equal(
    power_inner_join(list(df1, df2), df3, by = "id"),
    out
  )

  expect_equal(
    power_full_join(df1, list(df2, df3), by = "id"),
    out
  )

  expect_equal(
    power_full_join(list(df1, df2, df3), by = "id"),
    out
  )

  expect_equal(
    power_full_join(list(df1, df2), df3, by = "id"),
    out
  )

})
