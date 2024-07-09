#' Make [matrix] column names unique.
#'
#' @param prefix Prefix to add if column names are empty.
#' @return A [matrix] with updated column names.
#'
#' @keywords internal
make_colnames <- function(m, prefix = "X") {
  if (is.null(colnames(m))) {
    colnames(m) <- rep("", ncol(m))
  }
  if (all(colnames(m) == "")) {
    colnames(m) <- rep(prefix, ncol(m))
  }
  colnames(m) <- make.names(colnames(m), unique = TRUE)
  m
}

#' Shrinks a matrix with scores and adds an iteration column.
#'
#' @param m input matrix
#' @param last_iteration last iteration
#'
#' @return a [tibble::tibble()] wrapped in a list
#' @keywords internal
shrink_mat <- function(m, last_iteration) {
  if (is.null(m))
    return(m)
  dplyr::bind_cols(
    tibble::tibble(step=seq_len(last_iteration)),
    as.data.frame(utils::head(m, last_iteration))
  ) |>
    list()
}
