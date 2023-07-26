set.seed(42)
bc <- BatchContainer$new(
  dimensions = c(row = 16, column = 24)
)

samples <- data.frame(i = 1:384, x = rnorm(384))

test_that("assign_random shuffles elements", {
  bc <- assign_random(bc, samples)
  # this can fail by chance, but the probability extremely low
  # we set the random seed to make sure this is reproducible
  expect_true(any(bc$get_samples()$i != 1:384))
  bc <- assign_in_order(bc)
  expect_equal(bc$get_samples()$i, 1:384)
  bc <- assign_random(bc)
  expect_true(any(bc$get_samples()$i != 1:384))
})
