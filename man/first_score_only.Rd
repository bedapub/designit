% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_aggregation.R
\name{first_score_only}
\alias{first_score_only}
\title{Aggregation of scores: take first (primary) score only}
\usage{
first_score_only(scores, ...)
}
\arguments{
\item{scores}{A score or multiple component score vector}

\item{...}{Parameters to be ignored by this aggregation function}
}
\value{
The aggregated score, i.e. the first element of a multiple-component score vector.
}
\description{
This function enables comparison of the results of two scoring functions by just basing
the decision on the first element. This reflects the original behavior of the optimization
function, just evaluating the 'auxiliary' scores for the user's information.
}
\examples{
first_score_only(c(1, 2, 3))
}
