df1 <- data.frame(id = 1:2, val1 = 1:2)
df2 <- data.frame(id = 2:3, val2 = 2:3)

test_that("power_left_join works", {

  expect_equal(
    suppressMessages(power_left_join(df1, df2)),
    data.frame(id = 1:2, val1 = 1:2, val2 = c(NA, 2))
  )

  expect_message(
    power_left_join(df1, df2),
    "Joining")

  expect_equal(
    power_left_join(df1, df2, by = "id"),
    data.frame(id = 1:2, val1 = 1:2, val2 = c(NA, 2))
  )

  expect_error(
    power_left_join(df1, df2, by = "id", check = "B"),
    "check_specs"
  )
})

test_that("power_right_join works", {
  expect_equal(
    power_right_join(df1, df2, by = "id"),
    data.frame(id = 2:3, val1 = c(2, NA), val2 = 2:3)
  )
  expect_error(
    power_right_join(df1, df2, by = "id", check = "B"),
    "check_specs"
  )
})

test_that("power_inner_join works", {
  expect_equal(
    power_inner_join(df1, df2, by = "id"),
    data.frame(id = 2, val1 = 2, val2 = 2)
  )
  expect_error(
    power_inner_join(df1, df2, by = "id", check = "B"),
    "check_specs"
  )
})

test_that("power_full_join works", {
  expect_equal(
    power_full_join(df1, df2, by = "id"),
    data.frame(id = 1:3, val1 = c(1:2, NA), val2 = c(NA, 2:3))
  )
  expect_error(
    power_full_join(df1, df2, by = "id", check = "b"),
    "check_specs"
  )
})

test_that("dplyr code is still covered", {
  df1 <- data.frame(id = 1:2, val1 = 1:2)
  df2 <- data.frame(id = 2:3, val2 = 2:3)

  expect_equal(
    power_left_join(df1, df2, by = list(x = "id", y = "id")),
    data.frame(id = 1:2, val1 = 1:2, val2 = c(NA, 2))
  )

  expect_equal(
    power_right_join(df1, df2, by = "id", na_matches = "never"),
    data.frame(id = 2:3, val1 = c(2, NA), val2 = 2:3)
  )

  df1 <- data.frame(id1 = 1:2, val1 = 1:2)
  df2 <- data.frame(id2 = 2:3, val2 = 2:3)

  expect_error(
    power_inner_join(df1, df2),
    "no common var"
  )

  expect_error(
    power_inner_join(df1, df2, by = mean)
  )

  df1 <- data.frame(id = 1:2, val = 1:2)
  df2 <- data.frame(id = 2:3, val = 2:3)

  expect_message(
    power_inner_join(df1, df2),
    "Joining"
  )

  df1 <- data.frame(id = 1:2, val = 1:2)
  df2 <- data.frame(id = c("2", "3"), val = 2:3)

  expect_error(
    power_inner_join(df1, df2, by = "id"),
    "incompatible types"
  )
})

