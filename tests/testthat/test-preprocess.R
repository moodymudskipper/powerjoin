df1 <- data.frame(id = 1:2, val1 = 1:2)
df2 <- data.frame(id = 2:3, val2 = 2:3)

test_that("`select_keys_and` works", {
  expect_equal(
    power_inner_join(df1 %>% select_keys_and(val1), df2, by = "id"),
    data.frame(id = 2, val1 = 2, val2 = 2)
  )

  expect_equal(
    power_inner_join(df1 %>% select_keys_and(-val1), df2, by = "id"),
    data.frame(id = 2, val2 = 2)
  )
})

test_that("`summarize_by_keys` works", {
  expect_equal(
    power_inner_join(df1 %>% summarize_by_keys(val1 = max(val1)), df2, by = "id"),
    tibble(id = 2, val1 = 2, val2 = 2)
  )
})

test_that("`nest_by_keys` works", {
  expect_equal(
    power_inner_join(df1 %>% nest_by_keys(val1), df2, by = "id"),
    tibble(id = 2, val1 = list(2), val2 = 2)
  )

  expect_equal(
    power_inner_join(df1 %>% nest_by_keys(name = "foo"), df2, by = "id"),
    tibble(id = 2, foo = list(tibble(val1 = 2)), val2 = 2)
  )

  expect_equal(
    power_inner_join(df1 %>% nest_by_keys(val1, name = "foo"), df2, by = "id"),
    tibble(id = 2, foo = list(tibble(val1 = 2)), val2 = 2)
  )
})

test_that("`pack_along_keys` works", {
  expect_equal(
    power_inner_join(df1 %>% pack_along_keys(val1, name = "foo"), df2, by = "id"),
    tibble(id = 2, foo = tibble(val1 = 2), val2 = 2)
  )
})

test_that("`pivot_*_by_keys` works", {
  df_long <- tibble(id = c(1, 1, 2, 2), nm = c("a", "b", "a", "b"), val = 1:4)
  df_wide <- tibble(id = 1:2, a = c(1, 3), b = c(2, 4))
  df2 <- tibble(id = 1:2)
  expect_equal(
    power_inner_join(df_long %>% pivot_wider_by_keys(names_from = nm, values_from = val), df2, by = "id"),
    df_wide
  )
  expect_equal(
    power_inner_join(df_wide %>% pivot_longer_by_keys(names_to = "nm", values_to = "val"), df2, by = "id"),
    df_long
  )
})


