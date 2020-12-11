#' Shuffles samples trying to improve the scoring function.
#'
#' In every iteration shuffles several samples in the container.
#' If the `batch_container$score()` worsens, reverts to the previous state.
#'
#' @param batch_container An instance of `BatchContainer`.
#' @param samples A `data.frame` with sample information. Should be `NULL` if the `BatchContainer`
#' already has samples in it.
#' @param n_shuffle Number of times shuffling performed at each iterations.
#' Could be a number or a vector of length `iterations`.
#' In the later case, number of samples to shuffle could be precisely
#' set for every iteration. `n_shuffle == 1` indicates that two elements
#' are swapped one time. If `shuffle_proposal` function is provided, it will be
#' callend `n_shuffle` times at an iteration.
#' @param shuffle_proposal
#' A function used to propose two or more elements to shuffle in every step.
#' If non-`NULL` a function receives two arguments on every iteration:
#' `bc$samples_dt` and the iteration number. This function should return a list with attributes
#' `src` and `dst` (see [`BatchContainer$exchange_samples()`][BatchContainer]).
#' @param iterations Number of iterations.
#' @return A matrix with scores. Every row is an iteration. The matrix size is
#' `c(iterations, 1 + length(bc$aux_scoring_f))`.
#' @export
assign_score_optimize_shuffle <- function(batch_container, samples = NULL, n_shuffle = 1, shuffle_proposal = NULL, iterations = 1000) {
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
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
    msg = "n_shuffle should be an integer vector of length iteration or a single integer value or a function"
  )
  assertthat::assert_that(
    is.null(shuffle_proposal) || is.function(shuffle_proposal),
    msg = "shuffle_proposal should be a function"
  )

  if (length(n_shuffle) == 1) n_shuffle <- rep(n_shuffle, iterations)

  assertthat::assert_that(all(n_shuffle >= 1), msg = "n_shuffle values should be at least 1")

  assertthat::assert_that(!is.null(batch_container$scoring_f), msg = "no scoring function set for BatchContainer")
  current_score <- batch_container$score(aux = TRUE)
  scores <- matrix(0, nrow = iterations, ncol = length(current_score), dimnames = list(NULL, names(current_score)))

  for (i in seq(iterations)) {
    perm <- seq(n_avail)
    for (j in seq(n_shuffle[i])) {
      if (is.function(shuffle_proposal)) {
        sh <- shuffle_proposal(batch_container$samples_dt, i)
        src <- sh$src
        dst <- sh$dst
        if (is.null(src)) break
      } else {
        non_empty_loc <- which(!is.na(batch_container$samples_dt$.sample_id))
        assertthat::assert_that(length(non_empty_loc) > 0,
          msg = "all locations are empty in BatchContainer"
        )
        pos1 <- sample(non_empty_loc, 1)
        pos2 <- sample(which(seq(n_avail) != pos1), 1)
        src <- c(pos1, pos2)
        dst <- c(pos2, pos1)
      }
      perm[dst] <- perm[src]
      batch_container$exchange_samples(src, dst)
    }
    non_trivial <- which(perm != seq_along(perm))
    if (length(non_trivial) == 0) {
      message("no shuffling proposed, stopping the optimization")
      break
    }

    new_score <- batch_container$score(aux = TRUE)
    if (new_score[1] >= current_score[1]) {
      batch_container$exchange_samples(non_trivial, perm[non_trivial])
    } else {
      current_score <- new_score
    }

    scores[i, ] <- current_score
  }

  return(scores)
}
