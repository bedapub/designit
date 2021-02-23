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
#' set for every iteration. Please not that `n_shuffle` has a slightly different meaning depending
#' on whether you use `shuffle_proposal` or not. If `shuffle_proposal` is not set, this parameter
#' sets the number of items to shuffle (default 2). When `shuffle_proposal` is used, it will be
#' called `n_shuffle` times at an iteration (default 1).
#' @param shuffle_proposal
#' A function used to propose two or more elements to shuffle in every step.
#' If non-`NULL` a function receives two arguments on every iteration:
#' `bc$samples_dt` and the iteration number. This function should return a list with attributes
#' `src` and `dst` (see [`BatchContainer$exchange_samples()`][BatchContainer]).
#' @param iterations Number of iterations.
#' @return A matrix with scores. Every row is an iteration. The matrix size is
#' `c(iterations, 1 + length(bc$aux_scoring_f))`.
#' @export
assign_score_optimize_shuffle <- function(batch_container, samples = NULL, n_shuffle = NULL, shuffle_proposal = NULL, iterations = 1000) {
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
      msg = "batch-container is empty and no samples provided"
    )
  } else {
    assertthat::assert_that(nrow(samples) > 0)
    assign_random(batch_container, samples)
  }


  n_avail <- batch_container$n_available
  min_n_shuffle <- if (is.null(shuffle_proposal)) 2 else 1
  if (is.null(n_shuffle)) {
    n_shuffle <- min_n_shuffle
  }
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

  assertthat::assert_that(all(n_shuffle >= min_n_shuffle),
    msg = stringr::str_glue("n_shuffle values should be at least {min_n_shuffle}")
  )

  assertthat::assert_that(!is.null(batch_container$scoring_f), msg = "no scoring function set for BatchContainer")
  current_score <- batch_container$score(aux = TRUE)
  scores <- matrix(NA_real_, nrow = iterations, ncol = length(current_score), dimnames = list(NULL, names(current_score)))
  no_proposal <- FALSE

  for (i in seq_len(iterations)) {
    perm <- seq_len(n_avail)
    if (is.function(shuffle_proposal)) {
      for (j in seq_len(n_shuffle[i])) {
        sh <- shuffle_proposal(batch_container$samples_dt, i)
        assertthat::assert_that(is.list(sh), msg="Shuffle proposal function should return a list")
        src <- sh$src
        dst <- sh$dst
        if (is.null(src)) {
          if (j == 1) {
            no_proposal <- TRUE
          }
          break
        }
        perm[dst] <- perm[src]
        batch_container$exchange_samples(src, dst)
      }
    } else {
      non_empty_loc <- which(!is.na(batch_container$samples_dt$.sample_id))
      pos1 <- sample(non_empty_loc, 1)
      assertthat::assert_that(length(non_empty_loc) > 0,
        msg = "all locations are empty in BatchContainer"
      )
      pos_rest <- sample(which(seq_len(n_avail) != pos1), n_shuffle[i] - 1)
      src <- c(pos1, pos_rest)
      if (length(src) == 2) {
        # there is only one way to shuffle when there are two locations
        dst <- rev(src)
      } else {
        dst <- sample(src)
      }
      perm[dst] <- perm[src]
      batch_container$exchange_samples(src, dst)
    }

    if (no_proposal) {
      message("no shuffling proposed, stopping the optimization")
      scores <- scores[seq_len(i - 1), ]
      break
    }

    non_trivial <- which(perm != seq_along(perm))
    if (length(non_trivial) == 0) {
      # the shuffling is a trivial permutation, go to the next iteration
      scores[i, ] <- current_score
      next
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
