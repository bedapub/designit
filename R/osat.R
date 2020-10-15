#' Compute OSAT score for sample assignment. This implementation uses tibble.
#'
#' @param sample_assignment \code{tibble} or \code{data.frame} where every row is a location
#' in a container and a sample in this location.
#' @param batch_vars vector with batch variables to take into account for the
#' score computation.
#' @param feature_vars vector with sample variables to take into account for
#' score comptutation.
#' @param expected_df A \code{tibble} with expected number of samples sample variables and
#' batch variables combination. This is not required, however it does not change during the
#' optimization proccess. So it is a good idea to cache this value.
#'
#' @return a list with two attributes: res$score (numeric score value), res$expected_df
#' (expected counts \code{tibble} for potential)
#'
#' @examples
#' sample_assignment <- tibble::tribble(
#'   ~ID, ~SampleType, ~Race, ~plate,
#'   1, "Case", "Hispanic", 1,
#'   2, "Case", "Hispanic", 1,
#'   3, "Case", "European", 2,
#'   4, "Control", "Hispanic", 2,
#'   5, "Control", "European", 1,
#'   6, "Control", "European", 2,
#'   NA, NA, NA, 1,
#'   NA, NA, NA, 2,
#' )
#'
#' osat_score(sample_assignment,
#'   batch_vars = vars(plate),
#'   feature_vars = vars(SampleType, Race)
#' )
osat_score_tibble <- function(sample_assignment, batch_vars, feature_vars, expected_df = NULL) {
  assertthat::assert_that(is.data.frame(sample_assignment) && nrow(sample_assignment) > 0)
  special_col_names <- c(".n_batch", ".batch_freq", ".n_samples")
  special_col_names_str <- stringr::str_c(special_col_names, collapse = ", ")
  assertthat::assert_that(length(intersect(special_col_names, colnames(sample_assignment))) == 0,
    msg = glue::glue("special names ({special_col_names_str}) cannot be used as column names")
  )
  if (is.null(expected_df)) {
    batch_df <- sample_assignment %>%
      count(across({{ batch_vars }}), name = ".n_batch") %>%
      mutate(.freq_batch = .n_batch / sum(.n_batch)) %>%
      select(-.n_batch)

    features_df <- sample_assignment %>%
      count(across({{ feature_vars }}), name = ".n_samples") %>%
      drop_na()

    expected_df <- crossing(batch_df, features_df) %>%
      mutate(.n_expected = .n_samples * .freq_batch) %>%
      select(-.n_samples, -.freq_batch)
  } else {
    assertthat::assert_that(is.data.frame(expected_df) && nrow(expected_df) > 0)
    # I do not know how to check that columns parsed using tidy select are present
  }
  join_col_names <- sample_assignment %>%
    select(c({{ feature_vars }}, {{ batch_vars }})) %>%
    names()
  score <- sample_assignment %>%
    count(across(c({{ feature_vars }}, {{ batch_vars }})), name = ".n") %>%
    drop_na() %>%
    full_join(expected_df, by = join_col_names) %>%
    mutate(.n = replace_na(.n, 0)) %>%
    select(.n, .n_expected) %>%
    mutate(sq_diff = (.n - .n_expected)^2) %>%
    pull(sq_diff) %>%
    sum()
  list(score = score, expected_df = expected_df)
}


#' Compute OSAT score for sample assignment.
#'
#' @param sample_assignment \code{data.table} or \code{data.frame} where every row is a location
#' in a container and a sample in this location.
#' @param batch_vars \code{character} vector with batch variable names to take into account for the
#' score computation.
#' @param feature_vars \code{character} vector with sample variable names to take into account for
#' score comptation.
#' @param expected_dt A \code{data.table} with expected number of samples sample variables and
#' batch variables combination. This is not required, however it does not change during the
#' optimization proccess. So it is a good idea to cache this value.
#'
#' @return a list with two attributes: \code{$score} (numeric score value), \code{$expected_dt}
#' (expected counts \code{data.table} for reuse)
#' @export
#'
#' @examples
#' sample_assignment <- tibble::tribble(
#'   ~ID, ~SampleType, ~Race, ~plate,
#'   1, "Case", "Hispanic", 1,
#'   2, "Case", "Hispanic", 1,
#'   3, "Case", "European", 2,
#'   4, "Control", "Hispanic", 2,
#'   5, "Control", "European", 1,
#'   6, "Control", "European", 2,
#'   NA, NA, NA, 1,
#'   NA, NA, NA, 2,
#' )
#'
#' osat_score_datatable(sample_assignment,
#'   batch_vars = "plate",
#'   feature_vars = c("SampleType", "Race")
#' )
#' @importFrom data.table :=
osat_score <- function(df, batch_vars, feature_vars, expected_dt = NULL) {
  stopifnot(
    is.character(batch_vars),
    is.character(feature_vars)
  )
  assertthat::assert_that(is.data.frame(df) && nrow(df) > 0)
  df <- data.table::as.data.table(df)
  special_col_names <- c(".n_batch", ".batch_freq", ".n_samples")
  special_col_names_str <- stringr::str_c(special_col_names, collapse = ", ")
  assertthat::assert_that(length(intersect(special_col_names, colnames(df))) == 0,
    msg = glue::glue("special names ({special_col_names_str}) cannot be used as column names")
  )
  if (is.null(expected_dt)) {
    batch_df <- df[, .(.n_batch = .N), by = batch_vars]
    batch_df[, .freq_batch := .n_batch / sum(.n_batch), data.table::.SD]
    batch_df[, .n_batch := NULL]

    features_df <- na.omit(df)[, .(.n_samples = .N), by = feature_vars]

    # https://stackoverflow.com/a/14165493
    expected_dt <- data.table::setkey(batch_df[, c(k = 1, .SD)], k)[features_df[, c(k = 1, .SD)], allow.cartesian = TRUE][, k := NULL]
    expected_dt[, .n_expected := .n_samples * .freq_batch]
    expected_dt[, c(".n_samples", ".freq_batch") := NULL]

    data.table::setkeyv(expected_dt, c(batch_vars, feature_vars))
  } else {
    assertthat::assert_that(is.data.frame(expected_dt) && nrow(expected_dt) > 0)
    expected_colnames <- c(feature_vars, batch_vars) %>%
      c(".n_expected") %>%
      sort()
    expected_colnames_str <- stringr::str_c(expected_colnames, collapse = ", ")
    assertthat::assert_that(all(sort(colnames(expected_dt)) == expected_colnames),
      msg = glue::glue("expecting column names in expected_dt: {expected_colnames_str}")
    )
  }
  sample_count_df <- na.omit(df)[, .N, by = c(feature_vars, batch_vars)]
  data.table::setkeyv(sample_count_df, c(feature_vars, batch_vars))
  merged_df <- merge(sample_count_df, expected_dt, all = TRUE)
  merged_df[is.na(N), N := 0]
  score <- with(merged_df, sum((N - .n_expected)^2))
  list(score = score, expected_dt = expected_dt)
}
