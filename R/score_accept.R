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
  assertthat::assert_that(length(current_score) == length(best_score))
  all(current_score <= best_score) && any(current_score < best_score)
}


#' Alternative acceptance function for multi-dimensional scores in which order (left to right, e.g. first to last) denotes relevance.
#'
#' @param current_score One- or multi-dimensional score from the current optimizing iteration (double or vector of doubles)
#' @param best_score Best one- or multi-dimensional score found so far (double or vector of doubles)
#' @param tolerance Tolerance value: When comparing score vectors from left to right, differences within +/- tol won't immediately
#' shortcut the comparison at this point, allowing improvement in a less important score to exhibit some influence
#' @param ... Ignored arguments that may be used by alternative acceptance functions
#'
#' @return Boolean, TRUE if current score should be taken as the new optimal score, FALSE otherwise
#'
#' @export
accept_leftmost_improvement <- function(current_score, best_score, ..., tolerance = 0.0) {
  for (i in seq_along(current_score)) {
    if (current_score[i] > best_score[i] + tolerance) {
      return(FALSE)
    }
    if (current_score[i] < best_score[i] - tolerance) {
      return(TRUE)
    }
  }
  if (any(current_score < best_score)) TRUE else FALSE
}

#' Alternative acceptance function for multi-dimensional scores with exponentially downweighted score improvements from left to right
#'
#' @param kappa Coefficient that determines how quickly the weights for the individual score improvements drop when going from left to right
#' (i.e. first to last score). Weight for the first score's delta is 1, then the original delta multiplied with kappa^(p-1) for the p'th score
#' @param simulated_annealing Boolean; if TRUE, simulated annealing (SA) will be used to minimize the weighted improved score
#' @param temp_function In case SA is used, a temperature function that returns the annealing temperature for a certain iteration number
#'
#' @return Acceptance function which returns TRUE if current score should be taken as the new optimal score, FALSE otherwise
#'
#' @export
mk_exponentially_weighted_acceptance_func <- function(kappa = 0.5, simulated_annealing = FALSE,
                                                      temp_function = mk_simanneal_temp_func(T0 = 500, alpha = 0.8)) {
  force(kappa)
  force(temp_function)
  assertthat::assert_that(kappa > 0, kappa < 1, msg = "Exponential weighting parameter kappa has to be in the open inteval ]0,1[")
  if (simulated_annealing) {
    acc_func <- mk_simanneal_acceptance_func(temp_function = temp_function) # If SA, set up acceptance function with annealing protocol
  } else {
    acc_func <- function(current_score, ...) {
      current_score < 0
    } # else just check for an overall improvement of weighted sum of score differences
  }

  function(current_score, best_score, iteration) {
    weighted_imp <- sum((current_score - best_score) * kappa^(seq_along(current_score) - 1))

    acc_func(current_score = weighted_imp, best_score = 0.0, iteration) # Comparing the improvement against 0.0 is actually deciding on the delta
  }
}
