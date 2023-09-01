set.seed(43)

samples <- data.frame(
  id = 1:100,
  sex = sample(c("F", "M"), 100, replace = TRUE),
  group = sample(c("treatment", "control"), 100, replace = TRUE)
)

bc <- BatchContainer$new(
  dimensions = c("plate" = 5, "position" = 25)
)

scoring_f <- function(samples) {
  osat_score(
    samples,
    "plate",
    c("sex", "group")
  )$score
}

# in this example we treat all the positions in the plate as equal.
# when shuffling we enforce that source location is non-empty,
# and destination location has a different plate number
bc <- optimize_design(
  bc,
  scoring = scoring_f,
  samples,
  shuffle_proposal = shuffle_with_constraints(
    # source is non-empty location
    !is.na(.sample_id),
    # destination has a different plate
    plate != .src$plate
  ),
  max_iter = 10
)
