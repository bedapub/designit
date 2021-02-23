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
  expect_silent(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = 2))
  expect_silent(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = rep(c(3, 2), c(5, 5))))
  expect_silent(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = 10))
  # n_shuffle should be >= 2
  expect_error(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = 1))
  expect_error(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = 0))
  expect_error(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = c(rep(9, 9), 0)))
  expect_error(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = c(rep(9, 9), 1)))
})

test_that("error when shuffle proposal function returns not a list", {
  expect_error(
    assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = 2, shuffle_proposal = function(...) 0:1),
    "Shuffle proposal function should return a list"
  )
})

test_that("n_shuffle could be >=1 for shuffling with proposal function", {
  expect_silent(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = 1, shuffle_proposal = my_shuffle_proposal))
  expect_silent(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = rep(c(3, 1), c(5, 5)), shuffle_proposal = my_shuffle_proposal))
  expect_silent(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = 10, shuffle_proposal = my_shuffle_proposal))
  # n_shuffle should be >= 0
  expect_error(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = 0, shuffle_proposal = my_shuffle_proposal))
  expect_error(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = c(rep(9, 9), 0), shuffle_proposal = my_shuffle_proposal))
})

test_that("mismatch between number of iterations and n_shuffle", {
  expect_silent(trace <- assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = 2))
  expect_length(trace, 10)
  expect_error(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = c(2, 2)))
  expect_error(assign_score_optimize_shuffle(bc, iterations = 10, n_shuffle = rep(2, 200)))
})

test_that("inferring iterations from n_shuffle", {
  expect_silent(trace <- assign_score_optimize_shuffle(bc, n_shuffle = rep(2, 10)))
  expect_length(trace, 10)
})

test_that("default number of iterations", {
  expect_message(
    assign_score_optimize_shuffle(bc),
    "Number of iterations cannot be inferred; setting to 1000 iterations"
  )
})

test_that("default n_shuffle", {
  expect_silent(assign_score_optimize_shuffle(bc, iterations = 10))
  expect_silent(assign_score_optimize_shuffle(bc, iterations = 10, shuffle_proposal = my_shuffle_proposal))
})
