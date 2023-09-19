bc <- BatchContainer$new(
  dimensions = c(location = 20)
)

samples <- data.frame(
  sampleId = seq_len(bc$n_locations)
)

bc <- assign_random(bc, samples)


test_that("Score lenght is correct", {
  scoring_f <- list(
    function(...) rnorm(1)
  )
  expect_length(bc$score(scoring_f), 1)

  scoring_f <- list(
    function(...) rnorm(1),
    function(...) rnorm(1)
  )
  expect_length(bc$score(scoring_f), 2)
})

test_that("Score names are correct", {
  scoring_f <- list(
    a = function(...) rnorm(1),
    b = function(...) rnorm(1)
  )
  expect_named(bc$score(scoring_f), c("a", "b"))
})

test_that("Can optimize a single score", {
  set.seed(6)
  scoring_f <- function(...) rnorm(1)
  bc <- optimize_design(bc, scoring = scoring_f, max_iter = 30, n_shuffle = 2)
  expect_equal(
    nrow(bc$trace$scores[[1]]),
    31
  )
})

test_that("Can optimize multiple scores", {
  set.seed(6)
  scoring_f <- list(
    a = function(...) rnorm(1),
    b = function(...) rnorm(1)
  )
  bc <- optimize_design(bc, scoring = scoring_f, max_iter = 30, n_shuffle = 2, aggregate_scores_func = first_score_only)
  expect_equal(
    nrow(bc$trace$scores[[1]]),
    31
  )
})
