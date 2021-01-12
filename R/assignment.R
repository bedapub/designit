#' Assignment function which distributes samples randomly.
#'
#' @export
#' @param samples data.frame with samples.
#' @param batch_container Instance of BatchContainer class
#'
#' @return Returns `BatchContainer`, invisibly.
assign_random <- function(batch_container, samples = NULL) {
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
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

#' Distributes samples in order.
#'
#' First sample is assigned to the first location, second
#' sample is assigned to the second location, etc.
#'
#' @export
#' @param samples data.frame with samples.
#' @param batch_container Instance of BatchContainer class
#'
#' @return Returns `BatchContainer`, invisibly.
#' @example man/examples/assignment.R
assign_in_order <- function(batch_container, samples = NULL) {
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
      msg = "batch-container is empty and no samples provided"
    )
  } else {
    batch_container$samples_df <- samples
  }

  n_samples <- nrow(batch_container$samples_df)
  n_available <- batch_container$n_available

  assertthat::assert_that(n_available >= n_samples)

  batch_container$assignment_vec <- c(
    1:n_samples,
    rep(NA_integer_, n_available - n_samples)
  )

  invisible(batch_container)
}

#' Shuffling proposal function with constraints.
#'
#' Can be used with `assign_score_optimize_shuffle` to improve convergence speed.
#'
#' @export
#' @param src Expression to define possible source locations in the samples/locations
#' table. Usually evaluated based on `BatchContainer$samples_dt` as an environment
#' (see also `with()`). A single source location is selected from rows where the
#' expression evaluates to`TRUE`.
#' @param dst Expression to define possible destination locations in the
#' samples/locations table. Usually evaluated based on `BatchContainer$samples_dt` as an
#' environment.
#' Additionally a special variable `.src` is available in this environment which
#' describes the selected source row from the table.
#'
#' @return Returns a function which accepts a data.table (`dt`) and an iteration
#' number (`i`). This function returns a list with two names: `src` vector of length
#' 2 and `dst` vector of length two. See `BatchContainer$exchange_samples`.
#'
#' @example man/examples/shuffle_with_constraints.R
shuffle_with_constraints <- function(src = TRUE, dst = TRUE) {
  src <- enquo(src)
  dst <- enquo(dst)
  function(dt, i) {
    src_ind <- which(rlang::eval_tidy(src, dt))
    assertthat::assert_that(length(src_ind) > 0,
      msg = "source conditions not satisfied"
    )
    src_pos <- sample(src_ind, 1)
    # different from src
    dst_log <- seq(nrow(dt)) != src_pos
    env <- as.list(dt)
    env$.src <- dt[src_pos, ]
    dst_log <- dst_log & rlang::eval_tidy(dst, env)
    dst_ind <- which(dst_log)
    if (length(dst_ind) == 0) {
      warning("Cannot find destanation matching the constraints")
      list(src = NULL)
    } else {
      dst_pos <- sample(dst_ind, 1)
      list(
        src = c(src_pos, dst_pos),
        dst = c(dst_pos, src_pos)
      )
    }
  }
}

#' Distributes samples based on a sample sheet.
#'
#' @export
#' @param batch_container Instance of BatchContainer class
#' @param samples `data.frame` with samples (a sample sheet). This `data.frame`
#' (or `tibble::tibble()`) should contain samples together with their locations. No `.sample_id`
#' column can be present in the sample sheet. In `batch_container` already has samples assigned,
#' the function will check if samples in `batch_container` are identical to the ones in the
#' `samples` argument.
#'
#' @return Returns `BatchContainer`, invisibly.
#' @examples
#' bc <- BatchContainer$new(
#'   dimensions = list(
#'     plate = 2,
#'     column = list(values = letters[1:3]),
#'     row = 3
#'   )
#' )
#'
#' sample_sheet <- tibble::tribble(
#'   ~plate, ~column, ~row, ~sampleID, ~group,
#'   1, "a", 1, 1, "TRT",
#'   1, "b", 2, 2, "CNTRL",
#'   2, "a", 1, 3, "TRT",
#'   2, "b", 2, 4, "CNTR",
#'   2, "a", 3, 5, "TRT",
#' )
#' # assign samples from the sample sheet
#' assign_from_table(bc, sample_sheet)
#'
#' bc$get_samples(remove_empty_locations = TRUE)
#'
assign_from_table <- function(batch_container, samples) {
  # sample sheet has all the batch variable
  assertthat::assert_that(is.data.frame(samples) && nrow(samples) > 0,
                          msg = "samples should be non-empty data.frame")
  assertthat::assert_that(all(batch_container$dimension_names %in% colnames(samples)),
                          msg = "not all batch-container columns are present in the sample sheet")
  assertthat::assert_that(!".sample_id" %in% colnames(samples),
                          msg = "sample sheet should not have a reserved .sample_id column")
  location_columns <- batch_container$dimension_names
  sample_columns <- setdiff(colnames(samples), batch_container$dimension_names)
  only_samples <- samples[sample_columns] %>%
    # remove all-NA rows, i.e. unassigned locations
    dplyr::filter(!dplyr::across(everything(), is.na))
  if (is.null(batch_container$samples_df)) {
    batch_container$samples_df <- only_samples
  } else {
    assertthat::assert_that(dplyr::all_equal(only_samples,
                                             batch_container$get_samples(assignment=FALSE),
                                             ignore_col_order=TRUE,
                                             ignore_row_order=TRUE,
                                             convert=TRUE),
                            msg = "sample sheet should be compatible with samples inside the batch container")
  }
  only_locations <- samples[location_columns]
  assertthat::assert_that(nrow(dplyr::anti_join(only_locations, batch_container$locations_df,
                                                by=location_columns)) == 0,
                          msg = "sample sheed has locations not available in the batch container")
  samples_with_id <- batch_container$locations_df %>%
    dplyr::left_join(samples, by=location_columns) %>%
    dplyr::left_join(batch_container$samples_df, by=sample_columns)

  batch_container$assignment_vec <- samples_with_id$.sample_id

  invisible(batch_container)
}
