bc <- BatchContainer$new(
  dimensions = c(location = 20)
)

samples <- data.frame(
  sampleId = seq_len(bc$n_locations - 5)
)

bc <- assign_in_order(bc, samples)

set.seed(42)

test_that("empty locations are used by optimize_design", {
  # create 100 assignments
  assignments <- purrr::map_dfr(
    seq_len(20),
    function(...) {
      bc1 <- optimize_design(
        bc,
        scoring = function(...) rnorm(1),
        max_iter = 5
      )
      bc1$get_samples()
    },
    .id = "iteration"
  )

  assignments <- dplyr::filter(assignments, location > 15 & !is.na(sampleId))
  expect_gt(nrow(assignments), 0)
})
