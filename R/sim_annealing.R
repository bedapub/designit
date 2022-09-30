

#' Acceptance probability for a new solution
#'
#' A solution is always to be accepted if it leads to a lower score. Worse solutions should be accepted with a probability given by this function.
#'
#' @param current_score Score from the current optimizing iteration (scalar value, double)
#' @param best_score Score from the current optimizing iteration (scalar value, double)
#' @param temp Current value of annealing temperature
#' @param eps Small parameter eps(ilon), achieving that not always the new solution is taken when scores are exactly equal
#'
#' @return Probability with which to accept the new score as the best one
#'
#' @keywords internal
simanneal_acceptance_prob <- function(current_score, best_score, temp, eps = 0.1) {
  # Selected according to default function in
  # https://en.wikipedia.org/wiki/Simulated_annealing
  # but should stay fixed probably and could be included in the SA function to save one frequent function call

  if (current_score < best_score) {
    return(1)
  }
  if (current_score == Inf) {
    return(0)
  }
  exp((best_score - current_score - eps) / temp)
}



#' Create a temperature function that returns the annealing temperature at a given step (iteration)
#'
#' Supported annealing types are currently "Exponential multiplicative", "Logarithmic multiplicative", "Quadratic multiplicative" and "Linear multiplicative", each with dedicated constraints on alpha. For information, see http://what-when-how.com/artificial-intelligence/a-comparison-of-cooling-schedules-for-simulated-annealing-artificial-intelligence/
#'
#' @param T0 Initial temperature at step 1 (when k=0)
#' @param alpha Rate of cooling
#' @param type Type of annealing protocol. Defaults to the quadratic multiplicative method which seems to perform well.
#'
#' @return Temperature at cycle `k`.
#' @export
#'
#' @examples
mk_simanneal_temp_func <- function(T0, alpha, type = "Quadratic multiplicative") {
  force(T0)
  force(alpha)

  # May be re-parameterized so that k is within [0, 1] --> depending on maxiter, cooling can be slowed down accordingly
  # But: can only be done if the number of iterations is known beforehand

  switch(type,
    "Exponential multiplicative" = {
      assertthat::assert_that(alpha >= 0.8, alpha <= 0.9, msg = "Alpha has to be in interval [0.8, 0.9].")
      return(function(k) {
        T0 * alpha^k
      })
    },
    "Logarithmic multiplicative" = {
      assertthat::assert_that(alpha > 1, msg = "Alpha has to be >1.")
      return(function(k) {
        T0 / (1 + alpha * log(k + 1))
      })
    },
    "Quadratic multiplicative" = {
      assertthat::assert_that(alpha > 0, msg = "Alpha has to be > 0.")
      return(function(k) {
        T0 / (1 + alpha * k * k)
      })
    },
    "Linear multiplicative" = {
      assertthat::assert_that(alpha > 0, msg = "Alpha has to be > 0.")
      return(function(k) {
        T0 / (1 + alpha * k)
      })
    },
    stop("type of temperature function is not recognized.")
  )
}


#' Generate acceptance function for an optimization protocol based on simulated annealing
#'
#' @param temp_function A temperature function that returns the annealing temperature for a certain cycle k
#'
#' @return A function that takes parameters (`current_score`, `best_score`, `iteration`) for an optimization step and return a Boolean indicating whether the current solution should be accepted or dismissed. Acceptance probability of a worse solution decreases with annealing temperature.
#' @export
#'
#' @examples
mk_simanneal_acceptance_func <- function(temp_function = mk_simanneal_temp_func(T0 = 500, alpha = 0.8)) {
  force(temp_function)

  function(current_score, best_score, iteration) {
    assertthat::assert_that(length(current_score) == 1, msg = "Multi-dimensional scores have to be aggregated for simulated annealing to work.\nPlease specify a suitable aggregation function.")
    if (current_score < best_score) {
      return(TRUE)
    }
    simanneal_acceptance_prob(current_score, best_score, temp_function(iteration)) > stats::runif(1)
  }
}
