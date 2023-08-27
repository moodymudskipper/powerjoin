test_that("corner cases work", {
  # conflicts between key in x and col in y, key col is not suffixed, as with dplyr
  expect_equal(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo", a = "FOO"), c(a = "key")),
    dplyr::left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo", a = "FOO"), c(a = "key"))
  )

  # with keep = TRUE key col is renamed too
  expect_equal(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo", a = "FOO"), c(a = "key"), keep = TRUE),
    dplyr::left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo", a = "FOO"), c(a = "key"), keep = TRUE)
  )

  # the col is not renamed if we don't keep the key col
  expect_equal(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo", a = "FOO"), c(a = "key"), keep = "none"),
    data.frame(a = c("FOO", NA))
  )
  expect_equal(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo", a = "FOO"), c(a = "key"), keep = "right"),
    data.frame(key = c("foo", NA), a = c("FOO", NA))
  )

  ## same examples with fuzzy join

  # fuzzy join keep both cols and add suffixes
  expect_equal(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo", a = "FOO"), ~ .x$a == .y$key),
    data.frame(a.x = c("foo", "bar"), key = c("foo", NA), a.y = c("FOO", NA))
  )

  # we can handle these conflicts
  expect_equal(
    power_left_join(
      data.frame(a=c("foo", "bar")),
      data.frame(key = "foo", a = "FOO"),
      ~ .x$a == .y$key,
      conflict = coalesce),
    data.frame(key = c("foo", NA), a = c("foo", "bar"))
  )

  # the col is not renamed if we don't keep the key col
  expect_equal(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo", a = "FOO"),
                    ~ .x$a == .y$key, keep = "none"),
    data.frame(a = c("FOO", NA))
  )

  expect_equal(
    power_left_join(data.frame(a=c("foo", "bar")), data.frame(key = "foo", a = "FOO"),
                    ~ .x$a == .y$key, keep = "right"),
    data.frame(key = c("foo", NA), a = c("FOO", NA))
  )


})


# to do : tests that mix equi and fuzzy
