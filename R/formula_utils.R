#' Generate `terms.object` (formula with attributes)
#'
#' @param .tbl data
#' @param ... columns to skip (unquoted)
#'
#' @return [`terms.object`][stats::terms.object()]
#'
#' @export
#'
#' @examples
generate_terms <- function(.tbl, ...) {
  if (!tibble::is_tibble(.tbl)) .tbl <- tibble::as_tibble(.tbl)
  .tbl <- dplyr::select(.tbl, dplyr::everything(), ...)
  n <- nrow(.tbl)
  m <- ncol(.tbl)
  form <- paste("~ 1 + .")
  if (m > 1) form <- paste(form, "^", m) # .^1 doesn't work for no interaction (single variable)
  mf <- stats::as.formula(form)
  mt <- stats::terms.formula(mf, data = .tbl, simplify = TRUE)
  mm <- stats::model.matrix(mt, data = .tbl)
  # print(as.formula(mt))
  # print(ncol(mm))
  # print(n)
  # print(get_order(mt))
  while (ncol(mm) > n && get_order(mt) > 2) {
    # print('----')
    # print(as.formula(mt))
    # print(ncol(mm))
    # print(n)
    # print(get_order(mt))
    mt <- drop_order(mt)
    mm <- stats::model.matrix(mt, data = .tbl)
  }
  if (ncol(mm) > n) stop("dataset contains fewer rows than the main effects model.", call. = FALSE)
  mt
}

#' Drop highest order interactions
#'
#' @param .terms [`terms.object`][stats::terms.object()]
#' @param m order of interaction (highest available if -1)
#'
#' @return
#'
#' @export
#'
#' @examples
drop_order <- function(.terms, m = -1) {
  if (m == -1) m <- max(attr(.terms, "order"))
  if (m < 2) stop("there are no interactions left in the model.", call. = FALSE)
  stats::terms.formula(stats::reformulate(attr(.terms, "term.labels")[attr(.terms, "order") < m]))
}

#' Get highest order interaction
#'
#' @param .terms [`terms.object`][stats::terms.object()]
#'
#' @return highest order (numeric).
#' @export
#'
#' @examples
get_order <- function(.terms) {
  max(attr(.terms, "order"))
}

estimable <- function(x) {
  x <- as.matrix(x)
  QR <- qr(x)
  seq_len(ncol(x)) %in% QR$pivot[seq_len(QR$rank)]
}
