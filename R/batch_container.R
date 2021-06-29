#' Validates sample data.frame.
#'
#' @param samples A `data.frame` having a sample annotation per row.
validate_samples <- function(samples) {
  assertthat::assert_that(is.data.frame(samples),
    msg = "Samples should be a data.frame or tibble"
  )

  assertthat::assert_that(nrow(samples) == dplyr::n_distinct(samples),
    msg = "Non-unique rows in samples"
  )

  assertthat::assert_that(nrow(samples) >= 1,
    msg = "Samples should have at least one row"
  )

  assertthat::assert_that(nrow(dplyr::filter(samples, dplyr::across(tidyselect::everything(), is.na))) == 0,
                          msg = "Samples contain all-NA rows")
}



#' R6 Class representing a batch container.
#'
#' @description
#' Describes container dimensions and samples to container location assignment.
#'
#' @details
#' A typical workflow starts with creating a `BatchContainer`. Then
#' samples can be assigned to locations in that container.
#'
#' @export
BatchContainer <- R6::R6Class("BatchContainer",
  public = list(
    #' @description
    #' Create a new BatchContainer object.
    #' @param dimensions A vector or list of dimensions. Every dimension
    #' should have a name. Could be an integer vector of dimensions or
    #' a named list. Every value of a list could be either dimension size
    #' or parameters for [BatchContainerDimension$new()][BatchContainerDimension].
    #' @param exclude [data.frame] with excluded locations of a container.
    #' @examples
    #' bc <- BatchContainer$new(
    #'   dimensions = list(
    #'     "plate" = 3,
    #'     "row" = list(values = letters[1:3]),
    #'     "column" = list(values = c(1, 3))
    #'   ),
    #'   exclude = data.frame(plate = 1, row = "a", column = c(1, 3), stringsAsFactors = FALSE)
    #' )
    #'
    #' bc
    initialize = function(
                          dimensions,
                          exclude = NULL) {
      assertthat::assert_that(length(dimensions) >= 1)

      if (all(purrr::map_lgl(dimensions, inherits, "BatchContainerDimension")) &&
        is.null(names(dimensions))) {
        names(dimensions) <- purrr::map_chr(dimensions, ~ .$name)
      }

      assertthat::assert_that(!is.null(names(dimensions)),
        msg = "Dimensions should have names"
      )

      private$dimensions <- purrr::imap(dimensions, function(dm, name) {
        if (is.numeric(dm)) {
          assertthat::assert_that(!is.null(name), msg = "dimension should have a name")
          BatchContainerDimension$new(name = name, size = dm)
        } else if (inherits(dm, "BatchContainerDimension")) {
          assertthat::assert_that(
            dm$name == name,
            msg = "dimension names should match the list names"
          )
          dm
        } else {
          assertthat::assert_that(is.list(dm),
            msg = "Dimensions should be named numeric vector, list of BatchContainerDimension or list of lists/integers"
          )
          assertthat::assert_that(is.null(dm$name) || dm$name == name,
            msg = "dimension name should be set as a list name"
          )
          dm$name <- name
          do.call(BatchContainerDimension$new, dm)
        }
      })

      assertthat::assert_that(all(!duplicated(self$dimension_names)),
        msg = "duplicated dimension names"
      )

      self$exclude <- exclude
    },


    #' @description
    #' Return table with samples and sample assignment.
    #' @param assignment Return sample assignment. If FALSE, only
    #' samples table is returned, with out batch assignment.
    #' @param include_id Keep .sample_id in the tibble.
    #' @param remove_empty_locations Removes empty locations
    #' from the result tibble.
    #' @return tibble with samples and sample assignment.
    get_samples = function(assignment = TRUE, include_id = FALSE,
                           remove_empty_locations = FALSE) {
      assertthat::assert_that(!is.null(private$samples),
        msg = "Cannot return samples for empty batch container."
      )

      assertthat::assert_that(names(private$samples)[ncol(private$samples)] == ".sample_id",
        msg = "Last column of private$samples should be .sample_id"
      )


      assertthat::assert_that(assertthat::is.flag(assignment))
      assertthat::assert_that(assertthat::is.flag(include_id))
      assertthat::assert_that(assertthat::is.flag(remove_empty_locations))

      if (!assignment) {
        assertthat::assert_that(!remove_empty_locations,
          msg = "remove_empty_locations only makes sense when assignment is TRUE"
        )
      }
      if (assignment) {
        private$validate_assignment(private$assignment)
        res <- self$locations_df %>%
          dplyr::mutate(.sample_id = private$assignment) %>%
          dplyr::left_join(private$samples, by = ".sample_id")
        if (remove_empty_locations) {
          res <- res %>%
            dplyr::filter(!is.na(.sample_id))
        }
      } else {
        res <- private$samples
      }

      if (!include_id) {
        res <- res %>%
          dplyr::select(-.sample_id)
      }
      res
    },

    #' @description
    #' Exchange samples between locations
    #' @param src integer vector of source locations
    #' @param dst integer vector of destination locations (the same length as `src`).
    #' @return `BatchContainer`, invisibly
    exchange_samples = function(src, dst) {
      assertthat::assert_that(is.integer(src), length(src) > 0,
        is.integer(dst), length(dst) > 0,
        length(src) == length(dst),
        msg = "src & dst should be non-empty integer vectors of equal size"
      )
      # ensure private$samples_dt_cache is set
      self$samples_dt
      sid_ind <- match(".sample_id", colnames(private$samples_dt_cache))
      fcols <- colnames(private$samples_dt_cache)[sid_ind:ncol(private$samples_dt_cache)]
      val <- private$samples_dt_cache[src, fcols, with = FALSE]
      private$samples_dt_cache[dst, (fcols) := val]
      if (any(seq_len(nrow(private$samples)) != sort(private$samples_dt_cache$.sample_id))) {
        private$samples_dt_cache <- NULL
        stop("Samples lost or duplicated during exchange; check src and dst")
      }
      private$assignment <- private$samples_dt_cache$.sample_id
      invisible(self)
    },

    #' @description
    #' Score current sample assignment,
    #' @return Returns a vector of all scoring functions values.
    score = function() {
      assertthat::assert_that(!is.null(private$scoring_funcs),
        msg = "Scoring function needs to be assigned"
      )
      assertthat::assert_that(is.list(private$scoring_funcs),
                              length(private$scoring_funcs) >= 1,
        msg = "Scroring function should be a non-empty list"
      )
      assertthat::assert_that(!is.null(private$samples),
        msg = "No samples in the batch container, cannot compute score"
      )

      samples <- self$samples_dt
      res <- purrr::map_dbl(private$scoring_funcs, ~ .x(samples))
      assertthat::assert_that(length(res) == length(private$scoring_funcs))

      assertthat::assert_that(is.numeric(res), msg="Scoring function should return a number")

      return(res)
    },

    #' @description
    #' Prints information about `BatchContainer`.
    #' @param ... not used.
    print = function(...) {
      cat(stringr::str_glue(
        "Batch container with {self$n_locations} locations and {self$n_excluded} excluded.\n",
        .trim = FALSE
      ))
      cat("  Dimensions: ")
      private$dimensions %>%
        purrr::map_chr(~ .x$short_info) %>%
        stringr::str_c(collapse = ", ") %>%
        cat()
      cat("\n")
      invisible(self)
    }
  ),

  private = list(
    #' List of scoring functions.
    scoring_funcs = NULL,

    #' List of BatchContainerDimension elements.
    dimensions = NULL,

    #' Tibble with excluded locations.
    exclude_df = NULL,

    #' Tibble with sample information and sample ids.
    samples = NULL,

    #' Tibble with sample ids and assignment to batch container locations.
    assignment = NULL,

    #' Cached data.table with samples assignment.
    samples_dt_cache = NULL,

    #' Validate sample assignment.
    #' @importFrom stats na.omit
    validate_assignment = function(assignment) {
      assertthat::assert_that(is.integer(assignment),
        msg = "sample assignment should an integer vector"
      )
      assertthat::assert_that(length(assignment) == self$n_available,
        msg = "sample assignment length doesn't match the number of available locations"
      )
      assertthat::assert_that(!any(duplicated(na.omit(assignment))))
      assertthat::assert_that(length(intersect(1:nrow(self$samples_df), na.omit(assignment))) == sum(!is.na(assignment)),
        msg = "sample assignment does not match sample_ids (1..N_samples)"
      )
    }
  ),

  active = list(
    #' @field scoring_f
    #' Scoring functions used for optimization.
    #' Each scoring function should receive a [`data.table`][data.table::data.table] with columns from
    #' the samples [data.frame], locations [data.frame], and
    #' `.sample_id` column. This function should return a floating
    #' point score value for the assignment. This a list of functions.
    #' Upon assignment a single function will be automatically converted to a list
    #' In the later case each function is called.
    scoring_f = function(value) {
      if (missing(value)) {
        private$scoring_funcs
      } else {
        if (is.function(value)) {
          private$scoring_funcs <- list(value)
        } else {
          assertthat::assert_that(is.list(value), length(value) >= 1)
          assertthat::assert_that(
            all(purrr::map_lgl(self$scoring_f, is.function)),
            msg="All elements of scoring_f should be functions"
          )
          private$scoring_funcs <- value
        }
      }
    },

    #' @field has_samples
    #' Returns TRUE if `BatchContainer` has samples.
    has_samples = function(value) {
      if (missing(value)) {
        !is.null(private$samples)
      } else {
        stop("Cannot set has_samples (read-only).")
      }
    },

    #' @field n_locations
    #' Returns number of available locations in a `BatchContainer`.
    #' This field cannot be assigned.
    n_locations = function(value) {
      if (missing(value)) {
        private$dimensions %>%
          purrr::map_int(~ .x$size) %>%
          prod()
      } else {
        stop("Cannot set number of locations in a container (read-only).")
      }
    },

    #' @field n_excluded
    #' Returns number of excluded locations in a `BatchContainer`.
    #' This field cannot be assigned.
    n_excluded = function(value) {
      if (missing(value)) {
        if (is.null(private$exclude_df)) {
          0
        } else {
          nrow(private$exclude_df)
        }
      } else {
        stop("Cannot set number of excluded locations in container (read-only).")
      }
    },

    #' @field n_available
    #' Returns number of available locations in a `BatchContainer`.
    #' This field cannot be assigned.
    n_available = function(value) {
      if (missing(value)) {
        self$n_locations - self$n_excluded
      } else {
        stop("Cannot set dimension names (read-only).")
      }
    },

    #' @field n_dimensions
    #' Returns number of dimensions in a `BatchContainer`.
    #' This field cannot be assigned.
    n_dimensions = function(value) {
      if (missing(value)) {
        length(private$dimensions)
      } else {
        stop("Cannot set number of dimensions (read-only).")
      }
    },

    #' @field dimension_names
    #' [character] vector with dimension names.
    #' This field cannot be assigned.
    dimension_names = function(value) {
      if (missing(value)) {
        names(private$dimensions)
      } else {
        stop("Cannot set number of dimensions (read-only).")
      }
    },

    #' @field locations_df
    #' Get a [`tibble`][tibble::tibble()] with all the locations in a `BatchContainer`.
    #' This field cannot be assigned.
    locations_df = function(value) {
      if (missing(value)) {
        ldf <- private$dimensions %>%
          purrr::map(~ .x$values) %>%
          expand.grid() %>%
          dplyr::arrange(dplyr::across(dplyr::everything())) %>%
          tibble::as_tibble()
        if (!is.null(self$exclude)) {
          ldf <- dplyr::anti_join(ldf, self$exclude, by = self$dimension_names)
        }
        ldf
      } else {
        stop("Cannot set locations data.frame (read-only).")
      }
    },

    #' @field exclude
    #' Get or set excluded locations in the `BatchContainer`.
    exclude = function(value) {
      if (missing(value)) {
        private$exclude_df
      } else {
        # Change of exclude will also change n_available, which can
        # lead to the situation when there are more samples than locations.
        # Or if samples are assigned to location, we can have samples in
        # excluded locations.
        # In rare cases when this is really needed, one could create a new
        # BatchContainer.
        assertthat::assert_that(
           is.null(private$samples),
           msg = "Cannot change excluded locations after samples have been added"
        )
        if (is.null(value) || nrow(value) == 0) {
          private$exclude_df <- NULL
          return()
        }

        assertthat::assert_that(is.data.frame(value), msg = "Exclude should be a data.frame/tibble or NULL")

        assertthat::assert_that(setequal(colnames(value), names(private$dimensions)),
          msg = "Columns of exclude should match dimensions"
        )

        value <- value[, names(private$dimensions), drop = FALSE] %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric), as.integer),
            dplyr::across(where(is.factor), as.character)
          )

        rownames(value) <- NULL

        assertthat::assert_that(nrow(value) == dplyr::n_distinct(value),
          msg = "non-unique rows in exclude"
        )

        assertthat::assert_that(nrow(value) < self$n_locations,
          msg = "All locations in a container cannot be excluded"
        )

        for (dim_name in names(private$dimensions)) {
          assertthat::assert_that(
            all(value[[dim_name]] %in% private$dimensions[[dim_name]]$values),
            msg = stringr::str_glue("Some values are outside range in dimension '{dim_name}'")
          )
        }

        private$exclude_df <- tibble::as_tibble(value)
      }
    },

    #' @field samples_df
    #' Samples in the batch container.
    #' When assigning data.frame should not have column named .sample_id column.
    samples_df = function(samples) {
      if (missing(samples)) {
        private$samples
      } else {
        assertthat::assert_that(!is.null(samples),
          msg = "samples argument is NULL"
        )

        assertthat::assert_that(is.null(private$samples),
          msg = "batch container already has samples"
        )

        validate_samples(samples)

        assertthat::assert_that(nrow(samples) <= self$n_available,
          msg = "more samples than availble locations in the batch container"
        )

        assertthat::assert_that(nrow(samples) > 0 && ncol(samples) > 0,
                                msg = "samples should be a non-empty data.frame")

        assertthat::assert_that(length(intersect(self$dimension_names, colnames(samples))) == 0,
          msg = "some of the samples columns match batch container dimension names"
        )

        assertthat::assert_that(!".sample_id" %in% colnames(samples),
          msg = "samples data.frame has a column with reserved name .sample_id"
        )

        samples$.sample_id <- 1:nrow(samples)

        private$samples <- samples

        private$samples_dt_cache <- NULL
      }
    },

    #' @field samples_dt
    #' Sample assignment [`data.table`][data.table::data.table].
    #' This data.table includes columns for all the `BatchContainer`
    #' locations, all the samples and a `".sample_id"` column.
    #'
    #' The most efficient way of updating this [`data.table`][data.table::data.table] is using
    #' the `BatchContainer$exchange_samples()` method.
    samples_dt = function(value) {
      if (missing(value)) {
        if (is.null(private$samples_dt_cache)) {
          private$samples_dt_cache <- data.table::data.table(
            self$get_samples(include_id = TRUE)
          )
        }
        data.table::copy(private$samples_dt_cache)
      } else {
        stop("samples_dt is read-only.")
      }
    },

    #' @field assignment_vec
    #' Sample assignment vector. Should contain NAs for empty locations.
    assignment_vec = function(assignment) {
      if (missing(assignment)) {
        return(private$assignment)
      } else {
        private$validate_assignment(assignment)
        private$assignment <- assignment
        private$samples_dt_cache <- NULL
      }
    }
  ),
  cloneable = FALSE
)

BatchContainer$set("public", "clone", function() {
  bc <- BatchContainer$new(private$dimensions, private$exclude_df)
  if (!is.null(self$samples_df)) {
    bc$samples_df <- self$samples_df %>%
      dplyr::select(-.sample_id)
  }
  if (!is.null(self$assignment_vec)) {
    bc$assignment_vec <- self$assignment_vec
  }
  bc$scoring_f <- self$scoring_f
  bc
})
