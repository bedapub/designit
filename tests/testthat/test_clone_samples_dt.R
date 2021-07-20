test_that("$clone() does not preserve $samples_dt", {
  bc <- BatchContainer$new(
    dimensions = c(row = 3, column = 3)
  )
  samp <- data.frame(i = 1:9)
  assign_in_order(bc, samp)
  # creates cache
  bc$samples_dt
  bc_clone <- bc$clone()
  bc_clone$exchange_samples(c(1L, 2L), c(2L, 1L))

  expect_equal(bc$samples_dt$.sample_id, 1:9)
})

test_that("Cloning preservs dimensions, samples, scores & assignment", {
  bc <- BatchContainer$new(
    dimensions = c(row = 3, column = 3)
  )
  bc$exclude <- data.frame(row=3, column=1:3)
  samp <- data.frame(i = 1:6)
  assign_in_order(bc, samp)
  bc$scoring_f <- function(...) 42L
  bc_clone <- bc$clone()

  expect_equal(bc$samples, bc$samples)
  expect_equal(bc$locations, bc_clone$locations)
  expect_equal(bc$n_available, bc_clone$n_available)
  expect_equal(bc$score(), bc_clone$score())
  expect_equal(bc$n_dimensions, bc_clone$n_dimensions)
  expect_equal(bc$dimension_names, bc_clone$dimension_names)
  expect_equal(bc$get_samples(include_id=TRUE), bc_clone$get_samples(include_id=TRUE))
  expect_equal(bc$samples_dt, bc$samples_dt)
  expect_equal(bc$assignment_vec, bc_clone$assignment_vec)
  expect_equal(bc$exclude, bc$exclude)
})
