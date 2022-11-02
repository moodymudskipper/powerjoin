df1 <- data.frame(id = 1:2, val = 1:2)
df2 <- data.frame(id = 2:3, val = 3:4)

test_that("conflict works with functions", {
  expect_equal(
    power_inner_join(df1, df2, by = "id", conflict = `+`),
    data.frame(id = 2, val = 5)
  )

  expect_equal(
    power_inner_join(df1, df2, by = "id", conflict = ~ .x + .y),
    data.frame(id = 2, val = 5)
  )

  # row wise
  expect_equal(
    power_inner_join(df1, df2, by = "id", conflict = rw ~ sum(.x, .y)),
    data.frame(id = 2, val = 5)
  )

  expect_equal(
    power_inner_join(df1, df2, by = "id", conflict = c(val = `+`)),
    data.frame(id = 2, val = 5)
  )

  expect_equal(
    suppressWarnings(power_inner_join(df1, df2, by = "id", conflict = c(val = `+`, extra = `+`))),
    data.frame(id = 2, val = 5)
  )

  expect_warning(
    power_inner_join(df1, df2, by = "id", conflict = c(val = `+`, extra = `+`)),
    "conflict conditions are not used"
  )

  expect_error(
    power_inner_join(df1, df2, by = "id", conflict = "foo"),
    "wrong `conflict` argument"
  )

})


test_that("conflict works special values", {
  df1 <- data.frame(id = 1:3, val = 1:3)
  df2 <- data.frame(id = 1:2, val = c(2, NA))

  expect_equal(
    power_left_join(df1, df2, by = "id", conflict = coalesce_xy),
    data.frame(id = 1:3, val = 1:3)
  )

  expect_equal(
    power_left_join(df1, df2, by = "id", conflict = coalesce_yx),
    data.frame(id = 1:3, val = c(2, 2, 3))
  )

  expect_equal(
    power_left_join(df1, df2, by = "id", conflict = "patch"),
    data.frame(id = 1:3, val = c(2, NA, 3))
  )

})

test_that("conflict works with lists", {
  df1 <- data.frame(id = 1:3, val = 1:3)
  df2 <- data.frame(id = 1:2, val = c(2, NA))

  expect_equal(
    power_left_join(df1, df2, by = "id", conflict = list(val = coalesce_xy)),
    data.frame(id = 1:3, val = 1:3)
  )

  expect_equal(
    power_left_join(df1, df2, by = "id", conflict = list(val =coalesce_yx)),
    data.frame(id = 1:3, val = c(2, 2, 3))
  )

  expect_equal(
    power_left_join(df1, df2, by = "id", conflict = list(val="patch")),
    data.frame(id = 1:3, val = c(2, NA, 3))
  )

})
