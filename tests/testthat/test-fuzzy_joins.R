df1 <- data.frame(id = 1:2, val1 = 1:2)
df2 <- data.frame(id = 2:3, val2 = 2:3)

test_that("fuzzy joins work with `by` formulas", {

  df3 <- data.frame(id.x = c(1, 1:2), val1 = c(1, 1:2), id.y = c(2:3, 3), val2 = c(2:3, 3))

  # left
  expect_equal(
    power_left_join(df1, df2, by = ~ .x$id < .y$id),
    df3
  )

  # right (same thing here)
  expect_equal(
    power_right_join(df1, df2, by = ~ .x$id < .y$id),
    df3
  )

  # full (same thing again)
  expect_equal(
    power_full_join(df1, df2, by = ~ .x$id < .y$id),
    df3
  )

  # no match, this should not warn!
  expect_equal(
    power_inner_join(df1, df2, by = ~ .x$id > .y$id),
    df3[0,]
  )

  # zero match
  # fuzzyjoin::fuzzy_left_join(df1, df2, by = "id", match_fun = `>`)
  # fuzzyjoin::fuzzy_left_join(df1, df2, multi_by = list(x = "id", y = "id"), match_fun = NULL, multi_match_fun = ~ .x > .y)
  expect_equal(
    power_left_join(df1, df2, by = ~ .x$id > .y$id),
    data.frame(id.x = 1:2, val1 = 1:2, id.y = NA_integer_, val2 = NA_integer_)
  )

  # zero match + column creation
  # {fuzzyjoin} doesn't have the right behavior here, the column should be created
  # fuzzyjoin::fuzzy_left_join(df1, df2, multi_by = list(x = "id", y = "id"), match_fun = NULL, multi_match_fun = ~ .x > .y)
  expect_equal(
    power_left_join(df1, df2, by = ~ (foo <- .x$id > .y$id)),
    data.frame(id.x = 1:2, val1 = 1:2, id.y = NA_integer_, val2 = NA_integer_, foo = NA)
  )
})

test_that("fuzzy joins work with `by` lists()", {
  expect_equal(
    power_left_join(df1, df2, by = list(~ .x$id < .y$id)),
    data.frame(id.x = c(1, 1:2), val1 = c(1, 1:2), id.y = c(2:3, 3), val2 = c(2:3, 3))
  )
})

test_that("fuzzy joins can create columns", {
  expect_equal(
    power_left_join(df1, df2, by = list(~ (foo <- .x$id < .y$id))),
    data.frame(id.x = c(1, 1:2), val1 = c(1, 1:2), id.y = c(2:3, 3), val2 = c(2:3, 3), foo = TRUE)
  )
})
