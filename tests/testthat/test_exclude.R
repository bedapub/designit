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
  expect_equal(nrow(bc2$locations), 10 - 3)
  expect_equal(nrow(bc2$exclude), 3)
})

test_that("No way to exclude non-existing columns", {
  expect_error(bc1$exclude <- data.frame(row=2))
  expect_error(bc1$exclude <- data.frame(column=2))
  expect_error(bc2$exclude <- data.frame(wrong=2))
})

bc3 <- BatchContainer$new(
  dimensions = c(row = 10, column = 10),
)

samples <- data.frame(sampleId = 1:10)

test_that("Exclude only works before samples where assigned", {
  expect_null(bc3$exclude)
  expect_equal(bc3$n_available, 100)
  expect_equal(bc3$n_excluded, 0)
  bc3$exclude <- NULL
  expect_null(bc3$exclude)
  expect_equal(bc3$n_available, 100)
  expect_equal(bc3$n_excluded, 0)
  bc3$exclude <- data.frame(column = c(1,3), row=2)
  expect_equal(bc3$n_available, 100 - 2)
  bc3$exclude <- data.frame(column = 1, row=2)
  expect_equal(bc3$n_available, 100 - 1)
  bc3$samples <- samples
  expect_error(bc3$exclude <- data.frame(column = c(1,3), row=2))
  expect_equal(bc3$n_available, 100 - 1)
})
