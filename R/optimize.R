#' @description Swap two (more in the future) elements of the vector.
swap_elements <- function(vec, pos) {
  tmp <- vec[pos[1]]
  vec[pos[1]] <- vec[pos[2]]
  vec[pos[2]] <- tmp
  vec
}

#' Shuffles samples trying to improve the scoring function.
#'
#' In every iteration shuffles several samples in the container. If the `bc$score()` worsens,
#' reverts to the previous state.
#'
#' @param batch_container An instance of `BatchContainer`.
#' @param samples A `data.frame` with sample information. Should be `NULL` if the `BatchContainer`
#' already has samples in it.
#' @param n_shuffle Number of elements to swap in every iteration. *Note*: at the moment only 2 is
#' supported.
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
  assertthat::assert_that(n_shuffle == 2, msg = "n_shuffle > 2 not implemented yet")

  assertthat::assert_that(!is.null(batch_container$scoring_f), msg = "no scoring function set for BatchContainer")
  current_score <- batch_container$score(aux=TRUE)
  scores <- matrix(0, nrow = iterations, ncol = length(current_score), dimnames = list(NULL, names(current_score)))

  for (i in 1:iterations) {
    repeat {
      pos <- sample(n_avail, 2)

      # does not make sense to shuffle NAs
      if (any(!is.na(batch_container$assignment_vec[pos]))) {
        break
      }
    }

    batch_container$assignment_vec <- swap_elements(batch_container$assignment_vec, pos)

    new_score <- batch_container$score(aux = TRUE)
    if (new_score[1] >= current_score[1]) {
      batch_container$assignment_vec <- swap_elements(batch_container$assignment_vec, pos)
    } else {
      current_score <- new_score
    }

    scores[i, ] <- current_score
  }

  return(scores)
}
