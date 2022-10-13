bc <- BatchContainer$new(
  dimensions = c(location = 20)
)

samples <- data.frame(
  sampleId = seq_len(bc$n_locations)
)

assign_random(bc, samples)


test_that("Score lenght is correct", {
  bc$scoring_f <- list(
    function(...) rnorm(1)
  )
  expect_length(bc$scoring_f, 1)
  expect_length(bc$score(), 1)
  # single value is automatically converted to a list
  expect_type(bc$scoring_f, "list")

  bc$scoring_f <- function(...) rnorm(1)
  expect_length(bc$score(), 1)

  bc$scoring_f <- list(
    function(...) rnorm(1),
    function(...) rnorm(1)
  )
  expect_length(bc$score(), 2)
})

test_that("Score names are correct", {
  bc$scoring_f <- list(
    a = function(...) rnorm(1),
    b = function(...) rnorm(1)
  )
  expect_named(bc$score(), c("a", "b"))
})

test_that("Can optimize a single score", {
  set.seed(6)
  bc$scoring_f <- function(...) rnorm(1)
  expect_equal(
    optimize_design(bc, max_iter = 30, n_shuffle = 2)$n_steps,
    31
  )
})

test_that("Can optimize multiple scores", {
  set.seed(6)
  bc$scoring_f <- list(
    a = function(...) rnorm(1),
    b = function(...) rnorm(1)
  )
  expect_equal(
    optimize_design(bc, max_iter = 30, n_shuffle = 2, aggregate_scores_func = first_score_only)$n_steps,
    31
  )
})
