bcd1 <- BatchContainerDimension$new("d1", size = 10)
bcd2 <- BatchContainerDimension$new("d2", values = 1:10)
bcd3 <- BatchContainerDimension$new("d3", values = 2 + 1:10)
bcd4 <- BatchContainerDimension$new("d4", values = letters[1:10])
bcd5 <- BatchContainerDimension$new("d5", values = as.factor(letters[1:10]))

test_that("Test that dimension size is correct", {
  expect_equal(bcd1$size, 10)
  expect_equal(bcd2$size, 10)
  expect_equal(bcd3$size, 10)
  expect_equal(bcd4$size, 10)
  expect_equal(bcd5$size, 10)

  expect_equal(bcd1$size, length(bcd1$values))
  expect_equal(bcd2$size, length(bcd2$values))
  expect_equal(bcd3$size, length(bcd3$values))
  expect_equal(bcd4$size, length(bcd4$values))
  expect_equal(bcd5$size, length(bcd5$values))
})

test_that("Test that dimension name is correct", {
  expect_equal(bcd1$name, "d1")
  expect_equal(bcd2$name, "d2")
  expect_equal(bcd3$name, "d3")
  expect_equal(bcd4$name, "d4")
  expect_equal(bcd5$name, "d5")
})

test_that("Test that dimension values are set correctly", {
  expect_equal(bcd1$values, bcd2$values)
  expect_equal(bcd4$values, bcd5$values)
})

test_that("Test errors with invalid parameters", {
  # no name
  expect_error(BatchContainerDimension$new())
  expect_error(BatchContainerDimension$new(values = 1:10))
  expect_error(BatchContainerDimension$new(size = 10))

  # no size and values
  expect_error(BatchContainerDimension$new(name = "test"))

  # size a vector
  expect_error(BatchContainerDimension$new(name = "test", size = 1:10))

  # zero size
  expect_error(BatchContainerDimension$new(name = "test", size = 0))
  expect_error(BatchContainerDimension$new(name = "test", values = character()))
  expect_error(BatchContainerDimension$new(name = "test", values = numeric()))
  expect_error(BatchContainerDimension$new(name = "test", values = factor()))

  # NAs as values
  expect_error(BatchContainerDimension$new(name = "test", values = c(1:10, NA)))
})
