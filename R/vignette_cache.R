#' Load cache for a vignette chunk if it exists and
#' `params$regenerate_cache` is `FALSE`.
#'
#' This function will set the following variables in the global environment:
#' `.cache_filename_write`, `.eval_cached_chunk`.
#'
#' `.eval_cached_chunk` is set to `TRUE` if the chunk needs reevaluation.
#'
#' @param id A unique cache id.
#'
#' @keywords internal
#' @export
.load_cache <- function(id) {
  report_basename <- stringr::str_replace(knitr::current_input(), "[.]Rmd$", "")
  cache_filename <- stringr::str_c(report_basename, "_", id, ".rda")
  cache_filename_read <<- system.file("vignette_data",
    cache_filename,
    package = "designit"
  )
  .cache_filename_write <- NULL
  if (file.exists(cache_filename_read) && !params$regenerate_cache) {
    if (params$verbose_cache) {
      message(stringr::str_glue("Found {cache_filename_read}, loading"))
    }
    load(cache_filename_read, envir = .GlobalEnv)
    .eval_cached_chunk <<- FALSE
  } else {
    if (params$verbose_cache) {
      message(stringr::str_glue("Regenerating {cache_filename}"))
    }
    .cache_filename_write <<- file.path("..", "inst", "vignette_data", cache_filename)
    .eval_cached_chunk <<- TRUE
  }
  invisible(NULL)
}

#' Saves passed variables to `.cache_filename_write` if
#' `.cache_filename_write` is not `NULL` and `.eval_cached_chunk`
#' is `TRUE`
#' @keywords internal
#' @export
.save_cache <- function(...) {
  if (.eval_cached_chunk && !is.null(.cache_filename_write)) {
    if (params$verbose_cache) {
      message(stringr::str_glue("Saving cache to {.cache_filename_write}"))
    }
    save(..., file = .cache_filename_write)
  }
  invisible(NULL)
}
