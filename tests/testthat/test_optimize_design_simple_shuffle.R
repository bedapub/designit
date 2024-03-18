bc <- BatchContainer$new(
  dimensions = c(location = 1000)
)

samples <- data.frame(
  sampleId = seq_len(10)
)

env <- new.env()

n_elements_changed <- function(bc) {
  df <- bc$get_samples(include_id = TRUE, as_tibble = FALSE)
  cur_state <- df$.sample_id
  cur_state <- tidyr::replace_na(cur_state, -1)
  env$n_changed <- c(env$n_changed, sum(start_state != cur_state))
  # never accept the change
  Inf
}

bc <- assign_in_order(bc, samples)

scoring_f <- n_elements_changed

set.seed(42)

start_state <- ifelse(is.na(bc$assignment), -1, bc$assignment)

test_that("correct number of shuffles = 1", {
  env$n_changed <- numeric(0)
  optimize_design(
    bc,
    scoring = scoring_f,
    max_iter = 10,
    check_score_variance = F,
    autoscale_scores = F
  )
  expect_equal(env$n_changed, c(0, rep(2, 10)))
})

test_that("correct number of shuffles = 2", {
  env$n_changed <- numeric(0)
  optimize_design(
    bc,
    scoring = scoring_f,
    max_iter = 10,
    n_shuffle = 2,
    check_score_variance = F,
    autoscale_scores = F
  )
  expect_equal(env$n_changed, c(0, rep(4, 10)))
})

test_that("correct number of shuffles = 5", {
  env$n_changed <- numeric(0)
  optimize_design(
    bc,
    scoring = scoring_f,
    max_iter = 10,
    n_shuffle = 5,
    check_score_variance = FALSE,
    autoscale_scores = FALSE
  )
  expect_equal(env$n_changed, c(0, rep(10, 10)))
})

test_that("specify too many shuffles", {
  env$n_changed <- numeric(0)
  optimize_design(
    bc,
    scoring = scoring_f,
    max_iter = 10,
    n_shuffle = 40,
    check_score_variance = FALSE,
    autoscale_scores = FALSE
  )
  expect_equal(env$n_changed, c(0, rep(20, 10)))
})

test_that("complex shuffling schedule", {
  env$n_changed <- numeric(0)
  optimize_design(
    bc,
    scoring = scoring_f,
    max_iter = 10,
    n_shuffle = c(2, 2, 5, 2, 2, 10, 20, 40, 40),
    check_score_variance = F,
    autoscale_scores = F
  )
  expect_equal(env$n_changed, c(0, c(4, 4, 10, 4, 4, 20, 20, 20, 20)))
})
