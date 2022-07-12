

#' Default acceptance function. Accept current score if and only if all elements are less than or equal than in best score
#' and there's at least one improvement.
#'
#' @param current_score One- or multi-dimensional score from the current optimizing iteration (double or vector of doubles)
#' @param best_score Best one- or multi-dimensional score found so far (double or vector of doubles)
#' @param ... Ignored arguments that may be used by alternative acceptance functions
#'
#' @return Boolean, TRUE if current score should be taken as the new optimal score, FALSE otherwise
#'
#' @keywords internal
accept_strict_improvement <- function(current_score, best_score, ...) {

  # assertthat::assert_that(length(current_score)==length(best_score))

  if (length(current_score)==1) return(current_score < best_score)

  all(current_score <= best_score) && any(current_score < best_score)
}


#' Alternative acceptance function for optimizer. Accept current score if no )
#'
#' @param current_score One- or multi-dimensional score from the current optimizing iteration (double or vector of doubles)
#' @param best_score Best one- or multi-dimensional score found so far (double or vector of doubles)
#' @param tol Tolerance value
#' @param ... Ignored arguments that may be used by alternative acceptance functions
#'
#' @return Boolean, TRUE if current score should be taken as the new optimal score, FALSE otherwise
#'
#' @keywords internal
accept_leftmost_improvement <- function(current_score, best_score, tol=1e-6, ...) {
  for (i in seq_along(current_score)) {
    if (current_score[i] > best_score[i] + tol) return(FALSE)
    if (current_score[i] < best_score[i] - tol) return(TRUE)
  }
  if (any(current_score < best_score)) TRUE else FALSE
}
