bc <- BatchContainer$new(
  dimensions = c(location = 20)
)

samples <- data.frame(
  sampleId = seq_len(bc$n_locations)
)

bc <- assign_in_order(bc, samples)

test_that("mk_swapping_function returns an error if iteration number is too large", {
  f <- mk_swapping_function(c(1, 2, 3))
  expect_error(f(bc, 1), NA)
  expect_error(f(bc, 3), NA)
  expect_error(f(bc, 4))
})
