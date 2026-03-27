#' Compute distance to next occurrence of the same value in a vector
#'
#' For each element in the input vector, computes the distance (in positions)
#' to the next occurrence of the same value. If there is no next occurrence,
#' returns `Inf`.
#'
#' @param x A vector.
#'
#' @return A numeric vector of the same length as `x`, where each element
#'   is the distance to the next occurrence of the same value, or `Inf` if
#'   there is no next occurrence.
#'
#' @keywords internal
distance_to_next <- function(x) {
  out <- rep(Inf, length(x))
  pos_list <- split(seq_along(x), x)

  for (p in pos_list) {
    if (length(p) > 1) {
      out[p[-length(p)]] <- diff(p)
    }
  }

  out
}


#' Create a scoring function that penalizes small distances between
#' samples of the same category
#'
#' This scoring function is useful for ensuring even spacing of sample
#' categories along a one-dimensional layout, such as sample processing order.
#' It penalizes cases where samples of the same category are closer together
#' than the penalty threshold.
#'
#' The penalty for each pair of adjacent same-category samples is
#' `(penalty_threshold - actual_distance)^2` when
#' `actual_distance < penalty_threshold`, and 0 otherwise. The total score is
#' the sum of all penalties.
#'
#' @param feature_var Name of the column in the batch container samples
#'   that contains the feature/category to space out.
#' @param batch_var Name of the column in the batch container that defines
#'   the position/order of samples (e.g., processing order, time point).
#' @param penalty_threshold Distances smaller than this value will be
#'   penalized. If `NULL` (default), it is set to
#'   `floor(n_locations / n_levels)`, which is the ideal spacing for even
#'   distribution.
#'
#' @return A scoring function that takes a [BatchContainer] and returns
#'   a numeric score (lower is better).
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' samples <- data.frame(
#'   sample_id = 1:40,
#'   treatment = rep(paste0("Trt", 1:4), each = 10),
#'   sex = rep(c("F", "M"), 20)
#' )
#'
#' bc <- BatchContainer$new(
#'   dimensions = list(position = 40)
#' )
#' bc <- assign_random(bc, samples)
#'
#' # Create scoring functions
#' scoring <- list(
#'   treatment = mk_min_distance_score("treatment", "position"),
#'   sex = mk_min_distance_score("sex", "position")
#' )
#'
#' # Optimize
#' bc <- optimize_design(
#'   bc,
#'   scoring = scoring,
#'   aggregate_scores_func = sum_scores,
#'   max_iter = 500,
#'   quiet = TRUE
#' )
mk_min_distance_score <- function(
  feature_var,
  batch_var,
  penalty_threshold = NULL
) {
  force(feature_var)
  force(batch_var)
  force(penalty_threshold)

  function(bc) {
    samples <- bc$get_samples()

    assertthat::assert_that(
      feature_var %in% colnames(samples),
      msg = stringr::str_glue(
        "Column '{feature_var}' not found in batch container samples"
      )
    )
    assertthat::assert_that(
      batch_var %in% colnames(samples),
      msg = stringr::str_glue(
        "Column '{batch_var}' not found in batch container samples"
      )
    )

    # Order samples by batch_var
    samples <- samples[order(samples[[batch_var]]), ]
    feature_values <- samples[[feature_var]]

    # Compute penalty threshold if not provided
    thresh <- penalty_threshold
    if (is.null(thresh)) {
      n_locations <- length(feature_values)
      n_levels <- length(unique(feature_values))
      thresh <- floor(n_locations / n_levels)
    }

    # Compute distances to next occurrence
    d2n <- distance_to_next(feature_values)

    # Only penalize distances below threshold
    d2n <- d2n[d2n < thresh]

    # Penalty is defined as (threshold - distance)^2
    sum((thresh - d2n)^2)
  }
}
