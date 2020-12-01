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
    assertthat::assert_that(batch_container$has_samples(),
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
