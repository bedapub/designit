% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{invivo_study_samples}
\alias{invivo_study_samples}
\title{A sample list from an in vivo experiment with multiple treatments and 2 strains}
\format{
An object of class \code{"tibble"}
\describe{
\item{AnimalID}{The animal IDs, i.e. unique identifiers for each animal}
\item{Strain}{Strain (A or B)}
\item{Sex}{Female (F) or Male (M)}
\item{BirthDate}{Date of birth, not available for all the animals}
\item{Earmark}{Markings to distinguish individual animals, applied on the left (L), right (R) or both(B) ears}
\item{ArrivalWeight}{Initial body weight of the animal}
\item{Arrival weight Unit}{Unit of the body weight, here: grams}
\item{Litter}{The litter IDs, grouping offspring from one set of parents}
}
}
\usage{
data(invivo_study_samples)
}
\description{
This sample list is intended to be used in connection with the \code{"invivo_study_treatments"} data object
}
\author{
Guido Steiner
}
\keyword{datasets}
