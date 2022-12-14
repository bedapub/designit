% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permute_subgroups.R
\name{find_possible_block_allocations}
\alias{find_possible_block_allocations}
\title{Internal function to generate possible subgroup combinations that add up to specific levels of an allocation variable}
\usage{
find_possible_block_allocations(
  block_sizes,
  group_nums,
  fullTree = FALSE,
  maxCalls = 1e+06
)
}
\arguments{
\item{block_sizes}{(Integer) vector of sizes of the various subgroups that can be combined to form groups}

\item{group_nums}{Vector of sizes of the different groups to be formed}

\item{fullTree}{Boolean: Enforce full search of the possibility tree, independent of the value of \code{maxCalls}}

\item{maxCalls}{Maximum number of recursive calls in the search tree, to avoid long run times with very large trees}
}
\value{
List of possible allocations; Each allocation is an integer vector of allocation levels that are assigned in that order to the subgroups with sizes given by \verb{block sizes}
}
\description{
Internal function to generate possible subgroup combinations that add up to specific levels of an allocation variable
}
\keyword{internal}
