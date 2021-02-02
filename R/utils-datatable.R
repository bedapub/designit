#' This undocumented (?) flag allows us to use `:=`
#' without getting an error.
#' @keywords internal
#' @examples
#' \dontrun{
#' dt <- data.table::data.table(a=1, b=2)
#' dt[, a := NULL]
#' }
.datatable.aware = TRUE
