df1 <- data.frame(id = 1:2, val1 = 1:2)
df2 <- data.frame(id = 2:3, val2 = 2:3)

test_that("The `keep` argument works", {
  expect_equal(
    power_inner_join(df1, df2, by = "id", keep = TRUE),
    data.frame(id.x = 2, val1 = 2, id.y = 2, val2 = 2)
  )
})
