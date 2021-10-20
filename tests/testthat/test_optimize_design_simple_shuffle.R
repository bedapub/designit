bc <- BatchContainer$new(
  dimensions = c(location = 1000)
)

samples <- data.frame(
  sampleId = seq_len(10)
)

n_elements_changed <- function(df) {
  cur_state <- df$.sample_id
  cur_state <- tidyr::replace_na(cur_state, -1)
  n_changed <<- c(n_changed, sum(start_state != cur_state))
  # never accept the change
  Inf
}

assign_in_order(bc, samples)

bc$scoring_f <- n_elements_changed

set.seed(42)

start_state <- ifelse(is.na(bc$assignment), -1, bc$assignment)

n_changed <- numeric(0)
test_that("correct number of shuffles = 1", {
  optimize_design(
    bc,
    max_iter = 10
  )
  expect_equal(n_changed, c(0, rep(2, 10)))
})

n_changed <- numeric(0)
test_that("correct number of shuffles = 2", {
  optimize_design(
    bc,
    max_iter = 10,
    n_shuffle = 2
  )
  expect_equal(n_changed, c(0, rep(4, 10)))
})

n_changed <- numeric(0)
test_that("correct number of shuffles = 5", {
  optimize_design(
    bc,
    max_iter = 10,
    n_shuffle = 5
  )
  expect_equal(n_changed, c(0, rep(10, 10)))
})

n_changed <- numeric(0)
test_that("specify too many shuffles", {
  optimize_design(
    bc,
    max_iter = 10,
    n_shuffle = 40
  )
  expect_equal(n_changed, c(0, rep(20, 10)))
})

n_changed <- numeric(0)
test_that("complex shuffling schedule", {
  optimize_design(
    bc,
    max_iter = 10,
    n_shuffle = c(2, 2, 5, 2, 2, 10, 20, 40, 40)
  )
  expect_equal(n_changed, c(0, c(4, 4, 10, 4, 4, 20, 20, 20, 20)))
})

