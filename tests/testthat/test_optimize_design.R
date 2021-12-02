bc <- BatchContainer$new(
  dimensions = c(location = 20)
)

samples <- data.frame(
  sampleId = seq_len(bc$n_available)
)

assign_in_order(bc, samples)

bc$scoring_f <- function(...) rnorm(1)

my_shuffle_proposal <- function(...) {
  v <- sample(20, 2)
  list(src = v, dst = rev(v))
}

test_that("n_shuffle could be >=2 for simple shuffling", {
  expect_error(optimize_design(bc, max_iter = 10, n_shuffle = 2), NA)
  expect_error(optimize_design(bc, max_iter = 10, n_shuffle = rep(c(3, 2), c(5, 5))), NA)
  expect_error(optimize_design(bc, max_iter = 10, n_shuffle = 10), NA)
  # n_shuffle should be >= 1
  expect_error(optimize_design(bc, max_iter = 10, n_shuffle = 0))
  expect_error(optimize_design(bc, max_iter = 10, n_shuffle = c(rep(9, 9), 0)))
})

test_that("error when shuffle proposal function returns not a list", {
  expect_error(
    optimize_design(bc, max_iter = 10, n_shuffle = 2, shuffle_proposal = function(...) 0:1),
    "sample assignment length doesn't match the number of available locations"
  )
})

test_that("n_shuffle could be >=1 for shuffling with proposal function", {
  expect_error(
    optimize_design(bc, max_iter = 10, n_shuffle = 1, shuffle_proposal = my_shuffle_proposal),
    NA
  )
  expect_error(
    optimize_design(bc, max_iter = 10, n_shuffle = rep(c(3, 1), c(5, 5)), shuffle_proposal = my_shuffle_proposal),
    NA
  )
  expect_error(
    optimize_design(bc, max_iter = 10, n_shuffle = 10, shuffle_proposal = my_shuffle_proposal),
    NA
  )
  # n_shuffle should be >= 0
  expect_error(optimize_design(bc, max_iter = 10, n_shuffle = 0, shuffle_proposal = my_shuffle_proposal))
  expect_error(optimize_design(bc, max_iter = 10, n_shuffle = c(rep(9, 9), 0), shuffle_proposal = my_shuffle_proposal))
})

test_that("mismatch between number of iterations and n_shuffle", {
  expect_error(
    trace <- optimize_design(bc, max_iter = 5, n_shuffle = 2),
    NA
  )
  expect_equal(trace$n_steps, 5 + 1)
  expect_error(
    trace <- optimize_design(bc, max_iter = 10, n_shuffle = c(2, 2)),
    NA
  )
  expect_equal(trace$n_steps, 2 + 1)
  expect_error(
    trace <- optimize_design(bc, max_iter = 10, n_shuffle = rep(2, 200)),
    NA
  )
  expect_equal(trace$n_steps, 10 + 1)
})

test_that("inferring iterations from n_shuffle", {
  expect_error(
    trace <- optimize_design(bc, n_shuffle = rep(2, 5)),
    NA
  )
  expect_equal(trace$n_steps, 5 + 1)
})

test_that("default n_shuffle", {
  expect_error(
    optimize_design(bc, max_iter = 10),
    NA
  )
  expect_error(
    optimize_design(bc, max_iter = 10, shuffle_proposal = my_shuffle_proposal),
    NA
  )
})
