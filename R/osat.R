#' Compute OSAT score for sample assignment.
#'
#' The OSAT score is intended to ensure even distribution of samples across
#' batches and is closely related to the chi-square test contingency table
#' (Yan et al. (2012) \doi{10.1186/1471-2164-13-689}).
#'
#' @param bc [BatchContainer] with samples
#' or [`data.table`][data.table::data.table]/[data.frame] where every row is a location
#' in a container and a sample in this location.
#' @param batch_vars [character] vector with batch variable names to take into account for the
#' score computation.
#' @param feature_vars [character] vector with sample variable names to take into account for
#' score computation.
#' @param expected_dt A [`data.table`][data.table::data.table] with expected number of samples sample
#' variables and batch variables combination. This is not required, however it does not change
#' during the optimization process. So it is a good idea to cache this value.
#' @param quiet Do not warn about `NA`s in feature columns.
#'
#' @return a list with two attributes: `$score` (numeric score value), `$expected_dt`
#' (expected counts `data.table` for reuse)
#' @export
#'
#' @examples
#' sample_assignment <- tibble::tribble(
#'   ~ID, ~SampleType, ~Sex, ~plate,
#'   1, "Case", "Female", 1,
#'   2, "Case", "Female", 1,
#'   3, "Case", "Male", 2,
#'   4, "Control", "Female", 2,
#'   5, "Control", "Female", 1,
#'   6, "Control", "Male", 2,
#'   NA, NA, NA, 1,
#'   NA, NA, NA, 2,
#' )
#'
#' osat_score(sample_assignment,
#'   batch_vars = "plate",
#'   feature_vars = c("SampleType", "Sex")
#' )
#' @importFrom stats na.omit
osat_score <- function(bc, batch_vars, feature_vars, expected_dt = NULL, quiet = FALSE) {
  . <- .N <- `:=` <- .SD <- NULL # silence R check warnings
  .freq_batch <- .n_batch <- k <- .n_expected <- .n_samples <- N <- NULL # silence R check warnings
  stopifnot(
    is.character(batch_vars),
    is.character(feature_vars)
  )
  if (inherits(bc, "BatchContainer")) {
    df <- bc$get_samples(include_id = TRUE, as_tibble = FALSE)
  } else {
    assertthat::assert_that(is.data.frame(bc),
      msg = "bc should be a BatchContainer or a table"
    )
    df <- bc
  }
  assertthat::assert_that(is.data.frame(df) && nrow(df) > 0)
  df <- data.table::as.data.table(df)
  special_col_names <- c(".n_batch", ".batch_freq", ".n_samples")
  special_col_names_str <- stringr::str_c(special_col_names, collapse = ", ")
  assertthat::assert_that(length(intersect(special_col_names, colnames(df))) == 0,
    msg = stringr::str_glue(
      "special names ({special_col_names_str}) cannot be used as column names"
    )
  )
  if (is.null(expected_dt)) {
    batch_df <- df[, .(.n_batch = .N), by = batch_vars]
    batch_df[, .freq_batch := .n_batch / sum(.n_batch), data.table::.SD]
    batch_df[, .n_batch := NULL]

    features_df <- df[, .(.n_samples = .N), by = feature_vars]

    # https://stackoverflow.com/a/14165493
    expected_dt <- data.table::setkey(batch_df[, c(k = 1, .SD)], k)[features_df[, c(k = 1, .SD)], allow.cartesian = TRUE][, k := NULL]
    expected_dt[, .n_expected := .n_samples * .freq_batch]
    expected_dt[, c(".n_samples", ".freq_batch") := NULL]

    n_rows <- nrow(expected_dt)
    expected_dt <- na.omit(expected_dt)
    rows_removed <- n_rows - nrow(expected_dt)

    assertthat::assert_that(nrow(expected_dt) > 0,
      msg = "All elements of one of the features / batches are NAs"
    )
    if (rows_removed > 0 && !quiet) {
      warning("NAs in features / batch columns; they will be excluded from scoring")
    }

    data.table::setkeyv(expected_dt, c(batch_vars, feature_vars))
  } else {
    assertthat::assert_that(is.data.frame(expected_dt) && nrow(expected_dt) > 0)
    expected_colnames <- c(feature_vars, batch_vars) |>
      c(".n_expected") |>
      sort()
    expected_colnames_str <- stringr::str_c(expected_colnames, collapse = ", ")
    assertthat::assert_that(all(sort(colnames(expected_dt)) == expected_colnames),
      msg = stringr::str_glue(
        "expecting column names in expected_dt: {expected_colnames_str}"
      )
    )
  }
  sample_count_df <- na.omit(df[, .N, by = c(feature_vars, batch_vars)])
  data.table::setkeyv(sample_count_df, c(feature_vars, batch_vars))
  merged_df <- merge(sample_count_df, expected_dt, all = TRUE)
  merged_df[is.na(N), N := 0]
  score <- with(merged_df, sum((N - .n_expected)^2))
  list(score = score, expected_dt = expected_dt)
}


#' Convenience wrapper for the OSAT score
#'
#' This function wraps [osat_score()] in order to take full advantage of the speed gain without
#' managing the buffered objects in the user code.
#'
#' @param batch_vars [character] vector with batch variable names to take into account for the
#' score computation.
#' @param feature_vars [character] vector with sample variable names to take into account for
#' score computation.
#' @param quiet Do not warn about `NA`s in feature columns.
#' @return A function that returns the OSAT score for a specific sample arrangement
#' @export
#'
#' @examples
#' sample_assignment <- tibble::tribble(
#'   ~ID, ~SampleType, ~Sex, ~plate,
#'   1, "Case", "Female", 1,
#'   2, "Case", "Female", 1,
#'   3, "Case", "Male", 2,
#'   4, "Control", "Female", 2,
#'   5, "Control", "Female", 1,
#'   6, "Control", "Male", 2,
#'   NA, NA, NA, 1,
#'   NA, NA, NA, 2,
#' )
#'
#' osat_scoring_function <- osat_score_generator(
#'   batch_vars = "plate",
#'   feature_vars = c("SampleType", "Sex")
#' )
#'
#' osat_scoring_function(sample_assignment)
osat_score_generator <- function(batch_vars, feature_vars, quiet = FALSE) {
  force(batch_vars)
  force(feature_vars)

  expected_dt <- NULL

  first_call <- TRUE

  function(bc) {
    os <- osat_score(bc,
      batch_vars = batch_vars,
      feature_vars = feature_vars,
      expected_dt = expected_dt,
      quiet = quiet || !first_call
    )
    first_call <<- FALSE
    if (is.null(expected_dt)) expected_dt <<- os$expected_dt
    os$score
  }
}
