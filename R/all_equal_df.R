#' Compare two data.frames.
#'
#' This will convert factors to characters and disregard
#' row and column order
#'
#' @param df1 first [data.frame()] to compare
#' @param df2 second `data.frame()` to compare
#' @return `TRUE` or `FALSE` in case differences are present
#' @keywords internal
all_equal_df <- function(df1, df2) {
  if (!is.data.frame(df1) || !is.data.frame(df2)) {
    return(FALSE)
  }

  if (nrow(df1) != nrow(df2) || ncol(df1) != ncol(df2)) {
    return(FALSE)
  }

  assertthat::assert_that(
    !any(duplicated(colnames(df1))),
    !any(duplicated(colnames(df2))),
    msg = "duplicated colnames"
  )

  df2 <- df2[colnames(df1)]

  # convert factors to characters
  df1 <- df1 |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character))
  df2 <- df2 |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character))

  # order by all columns
  df1 <- df1[do.call(order, df1),]
  df2 <- df2[do.call(order, df2),]

  # remove row names
  rownames(df1) <- NULL
  rownames(df2) <- NULL

  assertthat::are_equal(
    all.equal(df1, df2, check.attributes = FALSE),
    TRUE
  )
}
