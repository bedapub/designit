bc <- BatchContainer$new(
  dimensions = c(location = 100)
)

samples <- data.frame(
  sampleId = seq_len(98)
)

bc <- assign_in_order(bc, samples)

# decreasing score
scoring_f <- (function() {
  score <- 1
  function(...) {
    score <<- score - 1
  }
})()

set.seed(42)

start_state <- ifelse(is.na(bc$assignment), -1, bc$assignment)

test_that("complete_random_shuffling shuffles most of the elements", {
  bc <- optimize_design(
    bc,
    scoring = scoring_f,
    max_iter = 1,
    shuffle_proposal_func = complete_random_shuffling
  )
  cur_state <- ifelse(is.na(bc$assignment), -1, bc$assignment)
  expect_lt(sum(start_state == cur_state), 50)
})
