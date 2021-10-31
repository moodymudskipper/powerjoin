test_that("check_specs methods work", {
  expect_output(
    print(check_specs()),
    "check specifications"
  )

  expect_equal(
    c(check_specs(column_conflict = "a", duplicate_keys_left = "a"),
      check_specs(duplicate_keys_left = "w", duplicate_keys_right = "w")),
    check_specs(column_conflict = "a", duplicate_keys_left = "w", duplicate_keys_right = "w")
  )
})


df1 <- data.frame(
  id1 = factor(c("a", "a", "b", "d")),
  id2 = c(1, 1, 3, NA),
  val = 1:4)
df2 <- data.frame(id1 = factor(c("a", "b", "c")), id2 = 1:3)

test_that("checks work", {

  expect_error(
    power_inner_join(df1, df2,
                     check = check_specs(implicit_keys = "a")),
    "explicit"
  )

  expect_error(
    power_inner_join(df1, df2, by = c("id1", "id2"),
                     check = check_specs(duplicate_keys_left = "a")),
    "duplicates"
  )

  expect_error(
    power_inner_join(df2, df1, by = c("id1", "id2"),
                     check = check_specs(duplicate_keys_right = "a")),
    "duplicates"
  )

  expect_error(
    power_inner_join(df1, df2, by = c("id1", "id2"),
                     check = check_specs(unmatched_keys_left = "a")),
    "unmatched"
  )

  expect_error(
    power_inner_join(df1, df2, by = c("id1", "id2"),
                     check = check_specs(unmatched_keys_right = "a")),
    "unmatched"
  )

  expect_error(
    power_inner_join(df1, df2, by = c("id1", "id2"),
                     check = check_specs(missing_key_combination_left = "a")),
    "combinations"
  )

  expect_error(
    power_inner_join(df1, df2, by = c("id1", "id2"),
                     check = check_specs(missing_key_combination_right = "a")),
    "combinations"
  )

  expect_error(
    power_inner_join(df1, df2, by = c("id1", "id2"),
                     check = check_specs(inconsistent_factor_levels = "a")),
    "different factor levels"
  )

  expect_message(
    power_inner_join(df1, rename(df2, ID2 = id2), by = c("id1", "id2" = "ID2"),
                     check = check_specs(inconsistent_factor_levels = "inform")),
    "different factor levels"
  )

  expect_error(
    power_inner_join(rename(df1, ID1 = id1), df2, by = c("ID1" = "id1", "id2"),
                     check = check_specs(inconsistent_factor_levels = "a")),
    "different factor levels"
  )

  expect_message(
    power_inner_join(rename(df1, ID1 = id1), df2, by = c("ID1" = "id1", "id2"),
                     check = check_specs(inconsistent_factor_levels = "inform")),
    "different factor levels"
  )

  expect_error(
    power_inner_join(df1, df2, by = c("id1", "id2"),
                     check = check_specs(inconsistent_type = "a")),
    "different type"
  )


  expect_error(
    power_inner_join(df1, rename(df2, ID2 = id2), by = c("id1", "id2" = "ID2"),
                     check = check_specs(inconsistent_type = "a")),
    "different type"
  )
})

