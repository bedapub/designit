#' Sample scores from a number of completely random sample permutations
#'
#' @param batch_container An instance of [BatchContainer].
#' @param scoring A named [list()] of scoring function. Each function should
#' return a vector of non-zero length.
#' @param random_perm Number of random sample permutations to be done.
#' @param sample_attributes_fixed Logical; if `FALSE`, simulate a shuffle function that alters sample attributes at each iteration.
#'
#' @return A score matrix with n (# of permutations) rows and m (dimensionality of score) columns.
#'
#' @keywords internal
sample_random_scores <- function(batch_container, scoring, random_perm, sample_attributes_fixed) {
  random_scores <- matrix(NA_real_, nrow = random_perm, ncol = length(batch_container$score(scoring)))
  for (i in seq_len(random_perm)) {
    batch_container$move_samples(location_assignment = complete_random_shuffling(batch_container))
    if (!sample_attributes_fixed && batch_container$has_samples_attr) {
      batch_container$samples_attr <- batch_container$samples_attr[sample(nrow(batch_container$samples_attr)), ]
    }
    random_scores[i, ] <- batch_container$score(scoring)
  }

  random_scores
}


#' Create a function that transforms a current (multi-dimensional) score into a boxcox normalized one
#'
#' @param batch_container An instance of [BatchContainer].
#' @param scoring A named [list()] of scoring function. Each function should
#' return a vector of non-zero length.
#' @param random_perm Number of random sample permutations for the estimation of the scaling params.
#' @param use_boxcox Logical; if TRUE and the `bestNormalize` package is available, boxcox transformations will be used to
#' normalize individual scores. If not possible, scores will just be transformed to a zero mean and unit standard deviation.
#' @param sample_attributes_fixed Logical; if FALSE, simulate a shuffle function that alters sample attributes at each iteration.
#'
#' @return The transformation function for a new score vector
#' @keywords internal
mk_autoscale_function <- function(batch_container, scoring, random_perm, use_boxcox = TRUE, sample_attributes_fixed = FALSE) {
  random_scores <- sample_random_scores(batch_container, scoring, random_perm, sample_attributes_fixed)
  score_dim <- length(batch_container$score(scoring))

  # Return function using boxcox transform if bestNormalize package is available
  if (use_boxcox && requireNamespace("bestNormalize", quietly = T)) {
    message("... Performing boxcox lambda estimation.")
    bc_transforms <- purrr::map(asplit(random_scores, 2), bestNormalize::boxcox, standardize = TRUE)

    return(
      function(score) {
        assertthat::assert_that(length(score) == score_dim)
        purrr::set_names(
          purrr::map2_dbl(bc_transforms, score, stats::predict),
          nm = names(score)
        )
      }
    )
  }


  # ... otherwise default to adjusting mean and StdDev to 0,1 respectively

  if (!use_boxcox) message("'bestNormalize' package not available.")
  message("... Performing simple mean/stddev adjustment.")
  mu <- purrr::map_dbl(asplit(random_scores, 2), mean, na.rm = T)
  sds <- purrr::map_dbl(asplit(random_scores, 2), stats::sd, na.rm = T)

  function(score) {
    assertthat::assert_that(length(score) == score_dim)
    purrr::set_names((score - mu) / sds, nm = names(score))
  }
}


#' Estimate the variance of individual scores by a series of completely random sample permutations
#'
#' @param batch_container An instance of `BatchContainer`.
#' @param scoring A named [list()] of scoring function. Each function should
#' return a vector of non-zero length.
#' @param random_perm Number of random sample permutations to be done.
#' @param sample_attributes_fixed Logical; if FALSE, simulate a shuffle function that alters sample attributes at each iteration.
#'
#' @return Vector of length m (=dimensionality of score) with estimated variances of each subscore
#'
#' @keywords internal
random_score_variances <- function(batch_container, scoring, random_perm, sample_attributes_fixed) {
  random_scores <- sample_random_scores(batch_container, scoring, random_perm, sample_attributes_fixed)
  purrr::map_dbl(asplit(random_scores, 2), stats::var, na.rm = T)
}
