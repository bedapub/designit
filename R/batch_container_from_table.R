#' Creates a [BatchContainer] from a table
#' ([data.frame]/[tibble::tibble]) containing sample and location information.
#'
#' @param tab A table with location and sample information.
#' Table rows with all `NA`s in sample information columns are treated as empty
#' locations.
#' @param location_cols Names of columns containing information about locations.
#'
#' @return A [BatchContainer] assigned samples.
#' @export
#'
#' @examples
batch_container_from_table <- function(tab, location_cols) {
  assertthat::assert_that(
    is.data.frame(tab)
  )
  assertthat::assert_that(
    !".sample_id" %in% colnames(tab),
    msg = ".sample_id cannot be a column name"
  )

  assertthat::assert_that(
    length(intersect(location_cols, colnames(tab))) == length(location_cols),
    msg = "Some column names in locations_cols are absent from tab"
  )
  batch_container <- BatchContainer$new(tab[location_cols])
  samples <- tab[setdiff(colnames(tab), location_cols)]
  assertthat::assert_that(
    ncol(samples) > 0,
    msg = "No sample information columns found in tab"
  )
  assertthat::assert_that(
    !all(is.na(samples)),
    msg = "Samples table is all-NAs"
  )

  not_na_rows <- rowSums(!is.na(samples)) > 0
  samples[not_na_rows, ".sample_id"] <- seq_len(sum(not_na_rows))

  batch_container$samples <- samples[
    not_na_rows,
    colnames(samples) != ".sample_id",
    drop = FALSE
  ]

  batch_container$move_samples(
    location_assignment = samples$.sample_id
  )
  batch_container
}
