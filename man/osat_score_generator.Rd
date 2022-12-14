% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osat.R
\name{osat_score_generator}
\alias{osat_score_generator}
\title{Convenience wrapper for the OSAT score}
\usage{
osat_score_generator(batch_vars, feature_vars, quiet = FALSE)
}
\arguments{
\item{batch_vars}{\link{character} vector with batch variable names to take into account for the
score computation.}

\item{feature_vars}{\link{character} vector with sample variable names to take into account for
score computation.}

\item{quiet}{Do not warn about \code{NA}s in feature columns.}
}
\value{
A function that returns the OSAT score for a specific sample arrangement
}
\description{
This function wraps \code{\link[=osat_score]{osat_score()}} in order to take full advantage of the speed gain without
managing the buffered objects in the user code.
}
\examples{
sample_assignment <- tibble::tribble(
  ~ID, ~SampleType, ~Sex, ~plate,
  1, "Case", "Female", 1,
  2, "Case", "Female", 1,
  3, "Case", "Male", 2,
  4, "Control", "Female", 2,
  5, "Control", "Female", 1,
  6, "Control", "Male", 2,
  NA, NA, NA, 1,
  NA, NA, NA, 2,
)

osat_scoring_function <- osat_score_generator(
  batch_vars = "plate",
  feature_vars = c("SampleType", "Sex")
)

osat_scoring_function(sample_assignment)
}
