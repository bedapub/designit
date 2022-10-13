#' Aggregation of scores: take first (primary) score only
#'
#' This function enables comparison of the results of two scoring functions by just basing
#' the decision on the first element. This reflects the original behavior of the optimization
#' function, just evaluating the 'auxiliary' scores for the user's information.
#'
#' @param scores A score or multiple component score vector
#' @param ... Parameters to be ignored by this aggregation function
#'
#' @return The aggregated score, i.e. the first element of a multiple-component score vector.
#'
#' @export
#'
#' @examples
#' first_score_only(c(1, 2, 3))
first_score_only <- function(scores, ...) {
  scores[1]
}


#' Aggregation of scores: take the maximum (i.e. worst score only)
#'
#' This function enables comparison of the results of two scoring functions by just basing
#' the decision on the largest element. This corresponds to the infinity-norm in ML terms.
#'
#' @param scores A score or multiple component score vector
#' @param na.rm Boolean. Should NA values be ignored when obtaining the maximum? FALSE by default as ignoring NA values may hide some issues with the provided scoring functions and also the aggregated value cannot be seen as the proper infinity norm anymore.
#' @param ... Parameters to be ignored by this aggregation function
#'
#' @return The aggregated score, i.e. the value of the largest element in a multiple-component score vector.
#'
#' @export
#'
#' @examples
#' worst_score(c(3, 2, 1))
worst_score <- function(scores, na.rm = FALSE, ...) {
  max(scores, na.rm = na.rm)
}


#' Aggregation of scores: sum up all individual scores
#'
#' @param scores A score or multiple component score vector
#' @param na.rm Boolean. Should NA values be ignored when obtaining the maximum? FALSE by default as ignoring NA values may render the sum meaningless.
#' @param ... Parameters to be ignored by this aggregation function
#'
#' @return The aggregated score, i.e. the sum of all indicidual scores.
#'
#' @export
#'
#' @examples
#' sum_scores(c(3, 2, 1))
sum_scores <- function(scores, na.rm = FALSE, ...) {
  sum(scores, na.rm = na.rm)
}


#' Aggregation of scores: L2 norm squared
#'
#' This function enables comparison of the results of two scoring functions by calculating
#' an L2 norm (euclidean distance from origin). Since this is only used for ranking solutions,
#' the squared L2 norm is returned.
#'
#' @param scores A score or multiple component score vector
#' @param ... Parameters to be ignored by this aggregation function
#'
#' @return The squared L2 norm as an aggregated score.
#'
#' @export
#'
#' @examples
#' L2s_norm(c(2, 2))
L2s_norm <- function(scores, ...) {
  sum(scores^2)
}

#' Aggregation of scores: L1 norm
#'
#' This function enables comparison of the results of two scoring functions by calculating
#' an L1 norm (Manhattan distance from origin).
#'
#' @param scores A score or multiple component score vector
#' @param ... Parameters to be ignored by this aggregation function
#'
#' @return The L1 norm as an aggregated score.
#'
#' @export
#'
#' @examples
#' L1_norm(c(2, 2))
L1_norm <- function(scores, ...) {
  sum(abs(scores))
}
