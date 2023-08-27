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


test_that("`complete_keys` works", {
  x1 <- data.frame(key = factor("a", levels = c("a", "b")), x = 1:2)
  y1 <- data.frame(key = c("b", "c"), y = 1:2, stringsAsFactors = FALSE)

  expect_equal(
    power_left_join(x1 %>% complete_keys(), y1, by = "key"),
    data.frame(key = c("a", "a", "b"), x = c(1, 2, NA), y = c(NA, NA, 1))
  )

  x2 <- data.frame(key1 = 1:2, key2 = 1:2, x = 1:2)
  y2 <- data.frame(key1 = 1, key2 = 2, y = 3)

  expect_equal(
    power_left_join(x2 %>% complete_keys(), y2, by = c("key1", "key2")),
    data.frame(key1 = c(1, 1, 2, 2), key2 = c(1:2, 1:2), x = c(1, NA, NA, 2), y = c(NA, 3, NA, NA))
  )
})

# test_that("`pivot_*_by_keys` works", {
#   df_long <- tibble(id = c(1, 1, 2, 2), nm = c("a", "b", "a", "b"), val = 1:4)
#   df_wide <- tibble(id = 1:2, a = c(1, 3), b = c(2, 4))
#   df2 <- tibble(id = 1:2)
#   expect_equal(
#     power_inner_join(df_long %>% pivot_wider_by_keys(names_from = nm, values_from = val), df2, by = "id"),
#     df_wide
#   )
#   expect_equal(
#     power_inner_join(df_wide %>% pivot_longer_by_keys(names_to = "nm", values_to = "val"), df2, by = "id"),
#     df_long
#   )
# })


