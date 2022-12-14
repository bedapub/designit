% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_annealing.R
\name{mk_simanneal_temp_func}
\alias{mk_simanneal_temp_func}
\title{Create a temperature function that returns the annealing temperature at a given step (iteration)}
\usage{
mk_simanneal_temp_func(T0, alpha, type = "Quadratic multiplicative")
}
\arguments{
\item{T0}{Initial temperature at step 1 (when k=0)}

\item{alpha}{Rate of cooling}

\item{type}{Type of annealing protocol. Defaults to the quadratic multiplicative method which seems to perform well.}
}
\value{
Temperature at cycle \code{k}.
}
\description{
Supported annealing types are currently "Exponential multiplicative", "Logarithmic multiplicative", "Quadratic multiplicative" and "Linear multiplicative", each with dedicated constraints on alpha. For information, see http://what-when-how.com/artificial-intelligence/a-comparison-of-cooling-schedules-for-simulated-annealing-artificial-intelligence/
}
