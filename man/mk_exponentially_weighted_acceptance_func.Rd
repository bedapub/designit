% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_accept.R
\name{mk_exponentially_weighted_acceptance_func}
\alias{mk_exponentially_weighted_acceptance_func}
\title{Alternative acceptance function for multi-dimensional scores with exponentially downweighted score improvements from left to right}
\usage{
mk_exponentially_weighted_acceptance_func(
  kappa = 0.5,
  simulated_annealing = FALSE,
  temp_function = mk_simanneal_temp_func(T0 = 500, alpha = 0.8)
)
}
\arguments{
\item{kappa}{Coefficient that determines how quickly the weights for the individual score improvements drop when going from left to right
(i.e. first to last score). Weight for the first score's delta is 1, then the original delta multiplied with kappa^(p-1) for the p'th score}

\item{simulated_annealing}{Boolean; if TRUE, simulated annealing (SA) will be used to minimize the weighted improved score}

\item{temp_function}{In case SA is used, a temperature function that returns the annealing temperature for a certain iteration number}
}
\value{
Acceptance function which returns TRUE if current score should be taken as the new optimal score, FALSE otherwise
}
\description{
Alternative acceptance function for multi-dimensional scores with exponentially downweighted score improvements from left to right
}
