# related to issue #35
bc <- BatchContainer$new(
  dimensions = c(location = 20)
)

samples <- data.frame(
  sampleId = seq_len(bc$n_available)
)

bc$scoring_f <- function(...) rnorm(1)

test_that("No shuffling proposed error is generated", {
  set.seed(6)
  expect_silent(
    assign_score_optimize_shuffle(bc, samples = samples, iterations = 30, n_shuffle = 2)
  )
})
