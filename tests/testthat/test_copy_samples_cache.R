test_that("$copy() does not preserve samples table cache", {
  bc <- BatchContainer$new(
    dimensions = c(row = 3, column = 3)
  )
  samp <- data.frame(i = 1:9)
  bc <- assign_in_order(bc, samp)
  # creates cache
  bc$get_samples()
  bc_clone <- bc$copy()
  bc_clone$move_samples(c(1L, 2L), c(2L, 1L))

  expect_equal(bc$get_samples(include_id = TRUE, as_tibble = FALSE)$.sample_id, 1:9)
})

test_that("Cloning preservs dimensions, samples, scores & assignment", {
  bc <- BatchContainer$new(
    dimensions = c(row = 3, column = 3),
    exclude = data.frame(row = 3, column = 1:3)
  )
  samp <- data.frame(i = 1:6)
  bc <- assign_in_order(bc, samp)
  scoring_f <- function(...) 42L
  bc_clone <- bc$copy()

  expect_equal(bc$samples, bc_clone$samples)
  expect_equal(bc$get_locations(), bc_clone$get_locations())
  expect_equal(bc$n_locations, bc_clone$n_locations)
  expect_equal(bc$score(scoring_f), bc_clone$score(scoring_f))
  expect_equal(bc$n_dimensions, bc_clone$n_dimensions)
  expect_equal(bc$dimension_names, bc_clone$dimension_names)
  expect_equal(bc$get_samples(include_id = TRUE), bc_clone$get_samples(include_id = TRUE))
  expect_equal(
    bc$get_samples(include_id = TRUE, as_tibble = FALSE),
    bc_clone$get_samples(include_id = TRUE, as_tibble = FALSE)
  )
  expect_equal(bc$get_samples(assignment = FALSE), bc_clone$get_samples(assignment = FALSE))
  expect_equal(bc$assignment, bc_clone$assignment)
  expect_equal(bc$exclude, bc_clone$exclude)
})
