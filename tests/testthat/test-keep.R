
test_that("The `keep` argument works with equi joins", {

  # equi join with keep = TRUE keep both id cols with suffixes
  expect_equal(
    power_full_join(
      data.frame(id = 1:2, val1 = 1:2),
      data.frame(id = 2:3, val2 = 2:3),
      by = "id", keep = TRUE),
    data.frame(
      id.x = c(1, 2, NA),
      val1 = c(1, 2, NA),
      id.y = c(NA, 2, 3),
      val2 = c(NA, 2, 3))
  )
  # keep = "both" is the same as `keep = TRUE`
  expect_equal(
    power_full_join(
      data.frame(id = 1:2, val1 = 1:2),
      data.frame(id = 2:3, val2 = 2:3),
      by = "id", keep = "both"),
    data.frame(
      id.x = c(1, 2, NA),
      val1 = c(1, 2, NA),
      id.y = c(NA, 2, 3),
      val2 = c(NA, 2, 3))
  )

  # if we use conflict functions key cols are merged
  expect_equal(
    power_full_join(
      data.frame(id = 1:2, val1 = 1:2),
      data.frame(id = 2:3, val2 = 2:3),
      by = "id", keep = TRUE, conflict = coalesce),
    data.frame(
      val1 = c(1, 2, NA),
      val2 = c(NA, 2, 3),
      id = c(1, 2, 3))
  )

  # equi join with keep = "default" merges key cols
  expect_equal(
    power_full_join(
      data.frame(id1=1, val1= "a"),
      data.frame(id2=2, val2= "b"),
      by = c(id1 = "id2"),
      keep = "default"),
    data.frame(id1 = 1:2, val1 = c("a", NA), val2 = c(NA, "b"))
  )

  # keep = FALSE is like keep = "default" for equi joins
  expect_equal(
    power_full_join(
      data.frame(id1=1, val1= "a"),
      data.frame(id2=2, val2= "b"),
      by = c(id1 = "id2"),
      keep = FALSE),
    data.frame(id1 = 1:2, val1 = c("a", NA), val2 = c(NA, "b"))
  )

  # keep = "left" keeps only the unmerged left keys
  expect_equal(
    power_full_join(
      data.frame(id1=1, val1= "a"),
      data.frame(id2=2, val2= "b"),
      by = c(id1 = "id2"),
      keep = "left"),
    data.frame(id1 = c(1, NA), val1 = c("a", NA), val2 = c(NA, "b"))
  )

  # keep = "right" keeps only the unmerged left keys
  expect_equal(
    power_full_join(
      data.frame(id1=1, val1= "a"),
      data.frame(id2=2, val2= "b"),
      by = c(id1 = "id2"),
      keep = "right"),
    data.frame(val1 = c("a", NA), id2 = c(NA, 2), val2 = c(NA, "b"))
  )

  # keep = "none" removes key columns
  expect_equal(
    power_full_join(
      data.frame(id1=1, val1= "a"),
      data.frame(id2=2, val2= "b"),
      by = c(id1 = "id2"),
      keep = "none"),
    data.frame(val1 = c("a", NA), val2 = c(NA, "b"))
  )
})

test_that("The `keep` argument works with fuzzy joins", {

  # left
  expect_equal(
    power_inner_join(
      data.frame(id1=1, val1= "a"),
      data.frame(id2=2, val2= "b"),
      by = ~ .x$id1 < .y$id2,
      keep = "left"),
    data.frame(id1 = 1, val1 = "a", val2 = "b")
  )

  # none
  expect_equal(
    power_inner_join(
      data.frame(id1=1, val1= "a"),
      data.frame(id2=2, val2= "b"),
      by = ~ .x$id1 < .y$id2,
      keep = "none"),
    data.frame(val1 = "a", val2 = "b")
  )

  # FALSE is like keep = "none" for fuzzy joins
  expect_equal(
    power_inner_join(
      data.frame(id1=1, val1= "a"),
      data.frame(id2=2, val2= "b"),
      by = ~ .x$id1 < .y$id2,
      keep = FALSE),
    data.frame(val1 = "a", val2 = "b")
  )

  # right
  expect_equal(
    power_inner_join(
      data.frame(id1=1, val1= "a"),
      data.frame(id2=2, val2= "b"),
      by = ~ .x$id1 < .y$id2,
      keep = "right"),
    data.frame(val1 = "a", id2 = 2, val2 = "b")
  )
})

