#' Assignment function which distributes samples randomly.
#'
#' @export
#' @param samples data.frame with samples.
#' @param batch_container Instance of BatchContainer class
#'
#' @return Returns `BatchContainer`, invisibly.
assign_random <- function(batch_container, samples = NULL) {
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples(),
      msg = "batch-container is empty and no samples provided"
    )
  } else {
    batch_container$samples_df <- samples
  }

  n_samples <- nrow(batch_container$samples_df)
  n_available <- batch_container$n_available

  assertthat::assert_that(n_available >= n_samples)

  expanded_ids <- c(
    1:n_samples,
    rep(NA_integer_, n_available - n_samples)
  )

  batch_container$assignment_vec <- sample(expanded_ids)

  invisible(batch_container)
}
