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



test_that("when creating columns through fuzzy joins, conflicts are handled", {
  # if we create a conflicting col it should be suffixed or handled
  expect_equal(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo"),
                    ~ .x$a == (a <- .y$key)),
    data.frame(a...1 = c("foo", "bar"), key = c("foo", NA), a...3 = c("foo", NA))
  )

  expect_error(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo"),
                    ~ .x$a == (a <- .y$key), check = check_specs(column_conflict = "abort")),
    "Conflicting columns"
  )

  expect_message(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo"),
                    ~ .x$a == (a <- .y$key), check = check_specs(column_conflict = "inform")),
    "Conflicting columns"
  )
})

test_that("mixing equi joins and fuzzy joins works", {
  df1 <- data.frame(id1 = c(1:2, 2), val = 1:3)
  df2 <- data.frame(id2 = c(2,2:3), val = 1:3, foo = 1)

  expect_equal(
    power_full_join(df1, df2, by = c("id1" = "id2", ~ .x$val > .y$val)),
    data.frame(id1 = c(2, 2, 2, 1, 3), val.x = c(2, 3, 3, 1, NA),
               val.y = c(1, 1, 2, NA, 3), foo = c(1, 1, 1, NA, 1))
  )

  expect_equal(
    power_full_join(df1, df2, by = c("id1" = "id2", ~ .x$val > .y$val), fill = 0),
    data.frame(id1 = c(2, 2, 2, 1, 3), val.x = c(2, 3, 3, 1, 0),
               val.y = c(1, 1, 2, 0, 3), foo = c(1, 1, 1, 0, 1))
  )

  expect_equal(
    power_full_join(df1, df2, by = c("id1" = "id2", ~ .x$val > .y$val), keep = "left"),
    data.frame(id1 = c(2, 2, 2, 1, NA), val = c(2, 3, 3, 1, NA), foo = c(1, 1, 1, NA, 1))
  )

  expect_equal(
    power_full_join(df1, df2, by = c("id1" = "id2", ~ .x$val > .y$val), keep = "right"),
    data.frame(id2 = c(2, 2, 2, NA, 3),
               val = c(1, 1, 2, NA, 3), foo = c(1, 1, 1, NA, 1))
  )

  expect_equal(
    power_full_join(df1, df2, by = c("id1" = "id2", ~ .x$val > .y$val), keep = "both"),
    data.frame(id1 = c(2, 2, 2, 1, NA), val.x = c(2, 3, 3, 1, NA),
               id2 = c(2, 2, 2, NA, 3), val.y = c(1, 1, 2, NA, 3), foo = c(1, 1, 1, NA, 1))
  )

  expect_equal(
    power_full_join(df1, df2, by = c("id1" = "id2", ~ .x$val > .y$val), keep = "none"),
    data.frame(foo = c(1, 1, 1, NA, 1))
  )
})
