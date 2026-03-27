test_that("distance_to_next computes correct distances", {
  # Simple case: alternating values

  expect_equal(
    designit:::distance_to_next(c("A", "B", "A", "B")),
    c(2, 2, Inf, Inf)
  )

  # All same value
  expect_equal(
    designit:::distance_to_next(c("A", "A", "A")),
    c(1, 1, Inf)
  )

  # All different values
  expect_equal(
    designit:::distance_to_next(c("A", "B", "C")),
    c(Inf, Inf, Inf)
  )

  # Mixed distances
  expect_equal(
    designit:::distance_to_next(c("A", "A", "B", "A", "B")),
    c(1, 2, 2, Inf, Inf)
  )
})


test_that("mk_min_distance_score returns zero for perfect spacing", {
  # 4 treatments, 8 positions -> ideal spacing is 2

  # Perfect alternation: Trt1, Trt2, Trt3, Trt4, Trt1, Trt2, Trt3, Trt4
  samples <- data.frame(
    sample_id = 1:8,
    treatment = rep(paste0("Trt", 1:4), 2),
    stringsAsFactors = FALSE
  )

  bc <- BatchContainer$new(dimensions = list(position = 8))
  bc <- assign_in_order(bc, samples)

  score_func <- mk_min_distance_score("treatment", "position")
  # All distances are 4, threshold is floor(8/4) = 2, so no penalties

  expect_equal(score_func(bc), 0)
})


test_that("mk_min_distance_score penalizes adjacent same-category samples", {
  # Adjacent samples of same treatment should be penalized

  samples <- data.frame(
    sample_id = 1:4,
    treatment = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE
  )

  bc <- BatchContainer$new(dimensions = list(position = 4))
  bc <- assign_in_order(bc, samples)

  score_func <- mk_min_distance_score("treatment", "position")
  # threshold = floor(4/2) = 2
  # distances: A->A = 1, B->B = 1
  # penalties: (2-1)^2 + (2-1)^2 = 2
  expect_equal(score_func(bc), 2)
})


test_that("mk_min_distance_score respects custom penalty_threshold", {
  samples <- data.frame(
    sample_id = 1:4,
    treatment = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )

  bc <- BatchContainer$new(dimensions = list(position = 4))
  bc <- assign_in_order(bc, samples)

  # Default threshold = floor(4/2) = 2, distances are all 2, so score = 0
  score_default <- mk_min_distance_score("treatment", "position")
  expect_equal(score_default(bc), 0)

  # Custom threshold = 3, distances are 2, so penalty = (3-2)^2 * 2 = 2
  score_custom <- mk_min_distance_score("treatment", "position", penalty_threshold = 3)
  expect_equal(score_custom(bc), 2)
})
