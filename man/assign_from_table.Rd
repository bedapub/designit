% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assignment.R
\name{assign_from_table}
\alias{assign_from_table}
\title{Distributes samples based on a sample sheet.}
\usage{
assign_from_table(batch_container, samples)
}
\arguments{
\item{batch_container}{Instance of BatchContainer class}

\item{samples}{\code{data.frame} with samples (a sample sheet). This \code{data.frame}
(or \code{tibble::tibble()}) should contain samples together with their locations. No \code{.sample_id}
column can be present in the sample sheet. In \code{batch_container} already has samples assigned,
the function will check if samples in \code{batch_container} are identical to the ones in the
\code{samples} argument.}
}
\value{
Returns a new \code{BatchContainer}.
}
\description{
Distributes samples based on a sample sheet.
}
\examples{
bc <- BatchContainer$new(
  dimensions = list(
    plate = 2,
    column = list(values = letters[1:3]),
    row = 3
  )
)

sample_sheet <- tibble::tribble(
  ~plate, ~column, ~row, ~sampleID, ~group,
  1, "a", 1, 1, "TRT",
  1, "b", 2, 2, "CNTRL",
  2, "a", 1, 3, "TRT",
  2, "b", 2, 4, "CNTRL",
  2, "a", 3, 5, "TRT",
)
# assign samples from the sample sheet
bc <- assign_from_table(bc, sample_sheet)

bc$get_samples(remove_empty_locations = TRUE)

}
