test_that("$clone() does not preserve private$samples_dt_cache", {
  bc <- BatchContainer$new(
    dimensions = c(row = 3, column = 3)
  )
  samp <- data.frame(i = 1:9)
  assign_in_order(bc, samp)
  # creates cache
  bc$get_samples()
  bc_clone <- bc$clone()
  bc_clone$exchange_samples(c(1L, 2L), c(2L, 1L))

  expect_equal(bc$get_samples(include_id=TRUE, as_tibble=FALSE)$.sample_id, 1:9)
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

  expect_equal(bc$samples, bc_clone$samples)
  expect_equal(bc$get_locations(), bc_clone$get_locations())
  expect_equal(bc$n_available, bc_clone$n_available)
  expect_equal(bc$score(), bc_clone$score())
  expect_equal(bc$n_dimensions, bc_clone$n_dimensions)
  expect_equal(bc$dimension_names, bc_clone$dimension_names)
  expect_equal(bc$get_samples(include_id=TRUE), bc_clone$get_samples(include_id=TRUE))
  expect_equal(bc$get_samples(include_id=TRUE, as_tibble=FALSE),
               bc_clone$get_samples(include_id=TRUE,as_tibble=FALSE))
  expect_equal(bc$get_samples(assignment=FALSE), bc_clone$get_samples(assignment=FALSE))
  expect_equal(bc$assignment, bc_clone$assignment)
  expect_equal(bc$exclude, bc_clone$exclude)
})
