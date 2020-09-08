#' Compute OSAT score for sample assignment.
#'
#' @param sample_assingment
#' @param batch_vars
#' @param feature_vars
#' @param expected_df
#'
#' @return a list with two attributes: res$score (numberic score value), res$expected_df (expected counts dataframe for potential reuse)
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
#' osat_score(sample_assignment,
#'   batch_vars = vars(plate),
#'   feature_vars = vars(SampleType, Race)
#' )
osat_score <- function(sample_assingment, batch_vars, feature_vars, expected_df = NULL) {
  stopifnot(
    is.list(batch_vars),
    is.list(feature_vars)
  )
  assertthat::assert_that(is.data.frame(sample_assingment) && nrow(sample_assingment) > 0)
  special_col_names <- c(".n_batch", ".batch_freq", ".n_samples")
  special_col_names_str <- stringr::str_c(special_col_names, collapse = ", ")
  assertthat::assert_that(length(intersect(special_col_names, colnames(sample_assignment))) == 0,
    msg = glue::glue("special names ({special_col_names_str}) cannot be used as column names")
  )
  if (is.null(expected_df)) {
    batch_df <- sample_assignment %>%
      select(!!!batch_vars) %>%
      count(!!!batch_vars, name = ".n_batch") %>%
      mutate(.freq_batch = .n_batch / sum(.n_batch)) %>%
      select(-.n_batch)

    features_df <- sample_assignment %>%
      count(!!!feature_vars, name = ".n_samples") %>%
      drop_na()

    expected_df <- crossing(batch_df, features_df) %>%
      mutate(.n_expected = .n_samples * .freq_batch) %>%
      select(-.n_samples, -.freq_batch)
  } else {
    assertthat::assert_that(is.data.frame(expected_df) && nrow(expected_df) > 0)
    expected_colnames <- c(feature_vars, batch_vars) %>%
      map_chr(as_label) %>%
      c(".n_expected") %>%
      sort()
    expected_colnames_str <- stringr::str_c(expected_colnames, collapse = ", ")
    assertthat::assert_that(all(sort(colnames(expected_df)) == expected_colnames),
      msg = glue::glue("expecting column names in expected_df: {expected_colnames_str}")
    )
  }
  score <- sample_assignment %>%
    count(!!!feature_vars, !!!batch_vars, name = ".n") %>%
    drop_na() %>%
    full_join(expected_df, by = map_chr(c(feature_vars, batch_vars), as_label)) %>%
    mutate(.n = replace_na(.n, 0)) %>%
    select(.n, .n_expected) %>%
    mutate(sq_diff = (.n - .n_expected)^2) %>%
    pull(sq_diff) %>%
    sum()
  list(score = score, expected_df = expected_df)
}
