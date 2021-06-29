bc1 <- BatchContainer$new(
  dimensions = c(row = 10, column = 10),
  exclude = data.frame(row=3, column=c(2,3))
)

test_that("Number available locations is computed correctly with exclude", {
  expect_equal(bc1$n_available, 100 - 2)
})

bc2 <- BatchContainer$new(
  dimensions = c(row = 10),
  exclude = data.frame(row=c(2, 3, 10))
)

test_that("Exclude works in 1-dimensional batch-container", {
  expect_equal(bc2$n_available, 10 - 3)
  expect_equal(nrow(bc2$locations_df), 10 - 3)
  expect_equal(nrow(bc2$exclude), 3)
})
