% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_plates.R
\name{mk_dist_matrix}
\alias{mk_dist_matrix}
\title{Internal helper function to set up an (n m) x (n m) pairwise distance matrix for a plate with n rows and m columns}
\usage{
mk_dist_matrix(
  plate_x = 12,
  plate_y = 8,
  dist = "minkowski",
  p = 2,
  penalize_lines = "soft"
)
}
\arguments{
\item{plate_x}{Dimension of plate in x direction (i.e number of columns)}

\item{plate_y}{Dimension of plate in y direction (i.e number of rows)}

\item{dist}{Distance function as understood by \code{stats::dist()}}

\item{p}{p parameter, used only if distance metric is 'minkowski'. Special cases: p=1 - Manhattan distance; p=2 - Euclidean distance}

\item{penalize_lines}{How to penalize samples of the same group in one row or column of the plate. Valid options are:
'none' - there is no penalty and the pure distance metric counts, 'soft' - penalty will depend on the well distance within the
shared plate row or column, 'hard' - samples in the same row/column will score a zero distance}
}
\value{
The  matrix with pairwise distances between any wells on the plate
}
\description{
Internal helper function to set up an (n m) x (n m) pairwise distance matrix for a plate with n rows and m columns
}
\keyword{internal}
