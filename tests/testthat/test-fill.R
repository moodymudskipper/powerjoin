df1 <- data.frame(id = 1:2, val1 = 1:2)
df2 <- data.frame(id = 2:3, val2 = 2:3)
test_that("fill  works", {
  expect_equal(
    power_full_join(df1, df2, by = "id", fill = 0),
    data.frame(id = 1:3, val1 = c(1:2, 0), val2 = c(0, 2:3))
  )

  expect_equal(
    power_full_join(df1, df2, by = "id", fill = list(val1 = 10, val2 = 20)),
    data.frame(id = 1:3, val1 = c(1:2, 10), val2 = c(20, 2:3))
  )
})

