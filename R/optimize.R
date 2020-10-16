#' Shuffles samples trying to improve the scoring function.
#'
#' In every iteration shuffles several samples in the container. If the `bc$score()` worsens,
#' reverts to the previous state.
#'
#' @param batch_container An instance of `BatchContainer`.
#' @param samples A `data.frame` with sample information. Should be `NULL` if the `BatchContainer`
#' already has samples in it.
#' @param n_shuffle Number of elements to swap in every iteration. Could be either a number or a
#' vector of length iteration. In the later case, number of samples to shuffle could be precisely
#' set for every iteration.
#' @param iterations Number of iterations.
#' @return A matrix with scores. Every row is an iteration. The matrix size is
#' `c(iterations, 1 + length(bc$aux_scoring_f))`.
#' @export
assign_score_optimize_shuffle <- function(batch_container, samples = NULL, n_shuffle = 2, iterations = 1000) {
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples(),
      msg = "batch-container is empty and no samples provided"
    )
  } else {
    assertthat::assert_that(nrow(samples) > 0)
    assign_random(batch_container, samples)
  }


  n_avail <- batch_container$n_available
  assertthat::assert_that(
    is.numeric(n_shuffle) &&
      (length(n_shuffle) == 1 || length(n_shuffle) == iterations),
    msg = "n_shuffle should be an integer vector of length iteration or a single integer value"
  )

  if (length(n_shuffle) == 1) n_shuffle <- rep(n_shuffle, iterations)

  assertthat::assert_that(all(n_shuffle > 1), msg = "n_shuffle values should be more than 1")

  assertthat::assert_that(!is.null(batch_container$scoring_f), msg = "no scoring function set for BatchContainer")
  current_score <- batch_container$score(aux = TRUE)
  scores <- matrix(0, nrow = iterations, ncol = length(current_score), dimnames = list(NULL, names(current_score)))

  for (i in 1:iterations) {
    repeat {
      src <- sample(n_avail, n_shuffle[i])

      # any of the locations is non-empty
      if (any(!is.na(batch_container$samples_dt$.sample_id[src]))) {
        break
      }
    }

    bc$exchange_samples(src)

    new_score <- batch_container$score(aux = TRUE)
    if (new_score[1] >= current_score[1]) {
      dst <- src[c(length(src), 1:(length(src) - 1))]
      bc$exchange_samples(src, dst)
    } else {
      current_score <- new_score
    }

    scores[i, ] <- current_score
  }

  return(scores)
}
