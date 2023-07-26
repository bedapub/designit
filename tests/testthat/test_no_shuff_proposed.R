# related to issue #35
bc <- BatchContainer$new(
  dimensions = c(location = 20)
)

samples <- data.frame(
  sampleId = seq_len(bc$n_locations)
)

scoring_f <- function(...) rnorm(1)

test_that("No shuffling proposed error is generated", {
  set.seed(6)
  expect_error(
    optimize_design(bc, scoring = scoring_f, samples = samples, max_iter = 30, n_shuffle = 2),
    NA
  )
})
