#' Assignment function which distributes samples randomly.
#'
#' @export
#' @param samples data.frame with samples.
#' @param batch_container Instance of BatchContainer class
#'
#' @return Returns a new `BatchContainer`.
#' @example man/examples/assignment.R
assign_random <- function(batch_container, samples = NULL) {
  batch_container <- assign_in_order(batch_container, samples)

  batch_container$move_samples(
    location_assignment = sample(batch_container$assignment)
  )

  batch_container
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
#' @return Returns a new `BatchContainer`.
#' @example man/examples/assignment.R
assign_in_order <- function(batch_container, samples = NULL) {
  batch_container <- batch_container$copy()
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
      msg = "batch-container is empty and no samples provided"
    )
  } else {
    batch_container$samples <- samples
  }

  n_samples <- nrow(batch_container$samples)
  n_locations <- batch_container$n_locations

  assertthat::assert_that(n_locations >= n_samples)

  batch_container$move_samples(location_assignment = c(
    1:n_samples,
    rep(NA_integer_, n_locations - n_samples)
  ))

  batch_container
}

#' Shuffling proposal function with constraints.
#'
#' Can be used with `optimize_design` to improve convergence speed.
#'
#' @export
#' @param src Expression to define possible source locations in the samples/locations
#' table. Usually evaluated based on
#' `BatchContainer$get_samples(include_id = TRUE, as_tibble = FALSE)` as an environment
#' (see also `with()`). A single source location is selected from rows where the
#' expression evaluates to`TRUE`.
#' @param dst Expression to define possible destination locations in the
#' samples/locations table. Usually evaluated based on `BatchContainer$get_samples()` as an
#' environment.
#' Additionally a special variable `.src` is available in this environment which
#' describes the selected source row from the table.
#'
#' @return Returns a function which accepts a `BatchContainer` and an iteration
#' number (`i`). This function returns a list with two names: `src` vector of length
#' 2 and `dst` vector of length two. See [`BatchContainer$move_samples()`][BatchContainer].
#'
#' @example man/examples/shuffle_with_constraints.R
shuffle_with_constraints <- function(src = TRUE, dst = TRUE) {
  src <- rlang::enquo(src)
  dst <- rlang::enquo(dst)
  function(bc, i) {
    dt <- bc$get_samples(include_id = TRUE, as_tibble = FALSE)
    src_ind <- which(rep_len(TRUE, nrow(dt)) & rlang::eval_tidy(src, dt))
    assertthat::assert_that(length(src_ind) > 0,
      msg = "source conditions not satisfied"
    )
    src_pos <- sample(src_ind, 1)
    # different from src
    dst_log <- seq_len(nrow(dt)) != src_pos
    env <- as.list(dt)
    env$.src <- dt[src_pos, ]
    dst_log <- dst_log & rlang::eval_tidy(dst, env)
    dst_ind <- which(dst_log)
    if (length(dst_ind) == 0) {
      warning("Cannot find destanation matching the constraints")
      list(src = NULL)
    } else {
      if (length(dst_ind) == 1) {
        dst_pos <- dst_ind
      } else {
        dst_pos <- sample(dst_ind, 1)
      }
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
#' @return Returns a new `BatchContainer`.
#'
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
#'   2, "b", 2, 4, "CNTRL",
#'   2, "a", 3, 5, "TRT",
#' )
#' # assign samples from the sample sheet
#' bc <- assign_from_table(bc, sample_sheet)
#'
#' bc$get_samples(remove_empty_locations = TRUE)
#'
assign_from_table <- function(batch_container, samples) {
  batch_container <- batch_container$copy()
  # sample sheet has all the batch variable
  assertthat::assert_that(is.data.frame(samples) && nrow(samples) > 0,
    msg = "samples should be non-empty data.frame"
  )
  assertthat::assert_that(all(batch_container$dimension_names %in% colnames(samples)),
    msg = "not all batch-container columns are present in the sample sheet"
  )
  assertthat::assert_that(!".sample_id" %in% colnames(samples),
    msg = "sample sheet should not have a reserved .sample_id column"
  )
  location_columns <- batch_container$dimension_names
  sample_columns <- setdiff(colnames(samples), batch_container$dimension_names)
  only_samples <- samples[sample_columns] |>
    # remove all-NA rows, i.e. unassigned locations
    dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na))
  if (is.null(batch_container$samples)) {
    batch_container$samples <- only_samples
  } else {
    assertthat::assert_that(all_equal_df(
      only_samples,
      batch_container$get_samples(assignment = FALSE)
    ),
    msg = "sample sheet should be compatible with samples inside the batch container"
    )
  }
  only_locations <- samples[location_columns]
  assertthat::assert_that(nrow(dplyr::anti_join(only_locations, batch_container$get_locations(),
    by = location_columns
  )) == 0,
  msg = "sample sheed has locations not available in the batch container"
  )
  samples_with_id <- batch_container$get_locations() |>
    dplyr::left_join(samples, by = location_columns) |>
    dplyr::left_join(batch_container$samples, by = sample_columns)

  batch_container$move_samples(location_assignment = samples_with_id$.sample_id)

  batch_container
}
