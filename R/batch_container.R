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

  assertthat::assert_that(nrow(dplyr::filter(samples, dplyr::if_all(tidyselect::everything(), is.na))) == 0,
    msg = "Samples contain all-NA rows"
  )
}

#' Create locations table from dimensions and exclude table
#'
#' @param dimensions A vector or list of dimensions. Every dimension
#' should have a name. Could be an integer vector of dimensions or
#' a named list. Every value of a list could be either dimension size
#' or parameters for [BatchContainerDimension$new()][BatchContainerDimension].
#' @param exclude [data.frame] with excluded locations of a container.
#'
#' @return a [tibble::tibble()] with all the available locations.
locations_table_from_dimensions <- function(dimensions, exclude) {
  assertthat::assert_that(length(dimensions) >= 1)

  if (all(purrr::map_lgl(dimensions, inherits, "BatchContainerDimension")) &&
    is.null(names(dimensions))) {
    names(dimensions) <- purrr::map_chr(dimensions, ~ .$name)
  }

  assertthat::assert_that(!is.null(names(dimensions)),
    msg = "Dimensions should have names"
  )

  dimensions <- purrr::imap(dimensions, function(dm, name) {
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

  assertthat::assert_that(all(!duplicated(names(dimensions))),
    msg = "duplicated dimension names"
  )

  ldf <- dimensions %>%
    purrr::map(~ .x$values) %>%
    expand.grid() %>%
    dplyr::arrange(dplyr::across(dplyr::everything())) %>%
    tibble::as_tibble()

  if (!is.null(exclude)) {
    assertthat::assert_that(
      is.data.frame(exclude),
      msg = "Exclude should be a data.frame/tibble or NULL"
    )

    assertthat::assert_that(setequal(colnames(exclude), names(dimensions)),
      msg = "Columns of exclude should match dimensions"
    )

    exclude <- exclude[, names(dimensions), drop = FALSE] %>%
      dplyr::mutate(
        dplyr::across(where(is.numeric), as.integer),
        dplyr::across(where(is.factor), as.character)
      )

    assertthat::assert_that(nrow(exclude) == dplyr::n_distinct(exclude),
      msg = "non-unique rows in exclude"
    )

    assertthat::assert_that(nrow(exclude) < nrow(ldf),
      msg = "All locations in a container cannot be excluded"
    )

    for (dim_name in names(dimensions)) {
      assertthat::assert_that(
        all(exclude[[dim_name]] %in% dimensions[[dim_name]]$values),
        msg = stringr::str_glue("Some values are outside range in dimension '{dim_name}'")
      )
    }

    exclude <- tibble::as_tibble(exclude)

    ldf <- dplyr::anti_join(ldf, exclude, by = colnames(exclude))
  }
  ldf
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
    #' @param locations_table A table with available locations.
    #' @param dimensions A vector or list of dimensions. Every dimension
    #' should have a name. Could be an integer vector of dimensions or
    #' a named list. Every value of a list could be either dimension size
    #' or parameters for
    #' [BatchContainerDimension$new()][BatchContainerDimension].
    #' Can be used as an alternative to passing `locations_table`.
    #' @param exclude [data.frame] with excluded locations of a container.
    #' Only used together with dimensions.
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
    initialize = function(locations_table,
                          dimensions,
                          exclude = NULL) {
      if (missing(locations_table)) {
        locations_table <- locations_table_from_dimensions(
          dimensions,
          exclude
        )
      } else {
        assertthat::assert_that(
          missing(dimensions) && is.null(exclude),
          msg = "dimensions and exclude cannot be used together with locations_table"
        )
      }
      assertthat::assert_that(
        is.data.frame(locations_table),
        msg = "Locations table should be a data.frame/tibble or NULL"
      )
      locations_table <- tibble::as_tibble(locations_table)

      assertthat::assert_that(
        nrow(locations_table) == dplyr::n_distinct(locations_table),
        msg = "non-unique rows in locations table"
      )

      assertthat::assert_that(
        all(colnames(locations_table) != ""),
        msg = "Location table column names should a non-empty strings"
      )

      assertthat::assert_that(
        !".sample_id" %in% colnames(locations_table),
        msg = "Cannot use reserved name for a location table column names (.sample_id)"
      )

      if (0 %in% rowSums(!is.na(locations_table))) {
        warning("Some batch container locations have only NA-attributes")
      }

      private$locations_df <- locations_table
    },


    #' @description
    #' Return table with samples and sample assignment.
    #' @param assignment Return sample assignment. If FALSE, only
    #' samples table is returned, with out batch assignment.
    #' @param include_id Keep .sample_id in the table. Use `TRUE` for
    #' lower overhead.
    #' @param remove_empty_locations Removes empty locations
    #' from the result tibble.
    #' @param as_tibble Return [`tibble`][tibble::tibble()].
    #' If `FALSE` returns [`data.table`][data.table::data.table()]. This should have
    #' lower overhead, as internally there is a cached [`data.table`][data.table::data.table()].
    #' @return table with samples and sample assignment.
    get_samples = function(assignment = TRUE, include_id = FALSE,
                           remove_empty_locations = FALSE,
                           as_tibble = TRUE) {
      assertthat::assert_that(!is.null(private$samples_table),
        msg = "Cannot return samples for empty batch container."
      )

      assertthat::assert_that(names(private$samples_table)[1] == ".sample_id",
        msg = "First column of private$samples_table should be .sample_id"
      )


      assertthat::assert_that(assertthat::is.flag(assignment))
      assertthat::assert_that(assertthat::is.flag(include_id))
      assertthat::assert_that(assertthat::is.flag(remove_empty_locations))
      assertthat::assert_that(assertthat::is.flag(as_tibble))

      if (!assignment) {
        assertthat::assert_that(!remove_empty_locations,
          msg = "remove_empty_locations only makes sense when assignment is TRUE"
        )
      }
      if (assignment) {
        if (is.null(private$samples_dt_cache)) {
          private$validate_assignment(private$assignment_vector)
          private$samples_dt_cache <- cbind(
            self$get_locations(),
            private$samples_table[private$assignment_vector, ]
          ) %>%
            data.table::as.data.table()
        }
        res <- data.table::copy(private$samples_dt_cache)
        if (remove_empty_locations) {
          res <- res[!is.na(.sample_id), ]
        }
      } else {
        res <- data.table::as.data.table(private$samples_table)
      }

      if (!is.null(private$samples_attributes)) {
        res <- cbind(res, private$samples_attributes[res$.sample_id])
      }

      if (!include_id) {
        res[, .sample_id := NULL]
      }

      if (as_tibble) {
        tibble::tibble(res)
      } else {
        res
      }
    },

    #' @description
    #' Get a table with all the locations in a `BatchContainer`.
    #' @return A [`tibble`][tibble::tibble()] with all the available locations.
    get_locations = function() {
      private$locations_df
    },


    #' @description
    #' Move samples between locations
    #'
    #' This method can receive either `src` and `dst` or `locations_assignment`.
    #'
    #' @param src integer vector of source locations
    #' @param dst integer vector of destination locations (the same length as `src`).
    #' @param location_assignment integer vector with location assignment.
    #' The length of the vector should match the number of locations,
    #' `NA` should be used for empty locations.
    #' @return `BatchContainer`, invisibly
    move_samples = function(src, dst, location_assignment) {
      if (!missing(src) && !is.null(src)) {
        assertthat::assert_that(missing(location_assignment) ||
          is.null(location_assignment),
        msg = "move_samples supports either src & dst, or location_assignment, not both"
        )
        assertthat::assert_that(
          # is.integer is much faster, but we want to allow
          # src = c(1, 2)
          # as opposed to
          # src = c(1L, 2L)
          rlang::is_integerish(src, finite = TRUE),
          length(src) > 0,
          rlang::is_integerish(dst, finite = TRUE),
          length(dst) > 0,
          length(src) == length(dst),
          msg = "src & dst should be non-empty integer vectors of equal size"
        )
        # ensure private$samples_dt_cache is set
        self$get_samples(include_id = TRUE, as_tibble = FALSE)
        sid_ind <- match(".sample_id", colnames(private$samples_dt_cache))
        fcols <- colnames(private$samples_dt_cache)[sid_ind:ncol(private$samples_dt_cache)]
        val <- private$samples_dt_cache[src, fcols, with = FALSE]
        private$samples_dt_cache[dst, (fcols) := val]
        if (any(seq_len(nrow(private$samples_table)) != sort(private$samples_dt_cache$.sample_id))) {
          private$samples_dt_cache <- NULL
          stop("Samples lost or duplicated during exchange; check src and dst")
        }
        private$assignment_vector <- private$samples_dt_cache$.sample_id
      } else {
        assertthat::assert_that(
          missing(src) || is.null(src),
          missing(dst) || is.null(dst),
          msg = "move_samples supports either src & dst, or location_assignment, not both"
        )
        private$validate_assignment(location_assignment)
        # if there is no cache yet
        if (is.null(private$samples_dt_cache)) {
          private$assignment_vector <- location_assignment
        } else {
          sid_ind <- match(".sample_id", colnames(private$samples_dt_cache))
          fcols <- colnames(private$samples_dt_cache)[sid_ind:ncol(private$samples_dt_cache)]
          val <- private$samples_table[location_assignment, ]
          private$samples_dt_cache[, (fcols) := val]
          private$assignment_vector <- private$samples_dt_cache$.sample_id
        }
      }
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
      assertthat::assert_that(!is.null(private$samples_table),
        msg = "No samples in the batch container, cannot compute score"
      )

      res <- purrr::map_dbl(private$scoring_funcs, ~ .x(self))
      assertthat::assert_that(length(res) == length(private$scoring_funcs))

      assertthat::assert_that(is.numeric(res), msg = "Scoring function should return a number")

      return(res)
    },

    #' @description
    #' Create an independent copy (clone) of a `BatchContainer`
    #' @return
    #' Returns a new `BatchContainer`
    copy = function() {
      # we do not name this method `clone` to avoid incorrect
      # autogenerated documentation (via roxygen2)
      bc <- BatchContainer$new(private$locations_df)
      if (!is.null(self$samples)) {
        bc$samples <- self$samples %>%
          dplyr::select(-.sample_id)
      }
      if (!is.null(self$assignment)) {
        bc$move_samples(location_assignment = self$assignment)
      }
      if (self$has_samples_attr) {
        bc$samples_attr <- private$samples_attributes
      }

      bc$scoring_f <- self$scoring_f
      bc
    },

    #' @description
    #' Prints information about `BatchContainer`.
    #' @param ... not used.
    print = function(...) {
      if (self$has_samples) {
        if (is.null(private$assignment_vector)) {
          assigned_info <- "unassigned"
        } else {
          assigned_info <- "assigned"
        }
        sample_info <- stringr::str_glue(
          " and {nrow(private$samples_table)} samples ({assigned_info})"
        )
      } else {
        sample_info <- ""
      }
      cat(stringr::str_glue(
        "Batch container with {self$n_locations} locations{sample_info}.\n",
        .trim = FALSE
      ))
      cat("  Dimensions: ")
      self$dimension_names %>%
        stringr::str_c(collapse = ", ") %>%
        cat()
      cat("\n")
      invisible(self)
    }
  ),
  private = list(
    #' List of scoring functions.
    scoring_funcs = NULL,

    #' Tibble with batch container locations.
    locations_df = NULL,

    #' Tibble with sample information and sample ids.
    samples_table = NULL,

    #' Sample attributes, a data.table.
    samples_attributes = NULL,

    #' Vector with assignment of sample ids to locations.
    assignment_vector = NULL,

    #' Cached data.table with samples assignment.
    samples_dt_cache = NULL,

    #' Validate sample assignment.
    #' @importFrom stats na.omit
    validate_assignment = function(assignment) {
      assertthat::assert_that(
        rlang::is_integerish(assignment),
        all(!is.infinite(assignment)),
        msg = "sample assignment should an integer vector without Infs"
      )
      assertthat::assert_that(length(assignment) == self$n_locations,
        msg = "sample assignment length doesn't match the number of available locations"
      )
      assertthat::assert_that(!any(duplicated(na.omit(assignment))))
      assertthat::assert_that(length(intersect(1:nrow(self$samples), na.omit(assignment))) == sum(!is.na(assignment)),
        msg = "sample assignment does not match sample_ids (1..N_samples)"
      )
    }
  ),
  active = list(
    #' @field scoring_f
    #' Scoring functions used for optimization.
    #' Each scoring function should receive a [BatchContainer].
    #' This function should return a floating
    #' point score value for the assignment. This a list of functions.
    #' Upon assignment a single function will be automatically converted to a list
    #' In the later case each function is called.
    scoring_f = function(value) {
      if (missing(value)) {
        private$scoring_funcs
      } else {
        if (is.null(value)) {
          private$scoring_funcs <- NULL
        } else if (is.function(value)) {
          private$scoring_funcs <- list(value)
        } else {
          assertthat::assert_that(is.list(value), length(value) >= 1)
          assertthat::assert_that(
            all(purrr::map_lgl(self$scoring_f, is.function)),
            msg = "All elements of scoring_f should be functions"
          )
          private$scoring_funcs <- value
        }
      }
    },

    #' @field has_samples
    #' Returns TRUE if `BatchContainer` has samples.
    has_samples = function(value) {
      if (missing(value)) {
        !is.null(private$samples_table)
      } else {
        stop("Cannot set has_samples (read-only).")
      }
    },

    #' @field has_samples_attr
    #' Returns TRUE if `BatchContainer` has sample atrributes assigned.
    has_samples_attr = function(value) {
      if (missing(value)) {
        !is.null(private$samples_attributes)
      } else {
        stop("Cannot set has_samples_attr (read-only).")
      }
    },

    #' @field n_locations
    #' Returns number of locations in a `BatchContainer`.
    n_locations = function(value) {
      if (missing(value)) {
        nrow(private$locations_df)
      } else {
        stop("Cannot set number of locations in a container (read-only).")
      }
    },

    #' @field n_dimensions
    #' Returns number of dimensions in a `BatchContainer`.
    #' This field cannot be assigned.
    n_dimensions = function(value) {
      if (missing(value)) {
        ncol(private$locations_df)
      } else {
        stop("Cannot set number of dimensions (read-only).")
      }
    },

    #' @field dimension_names
    #' [character] vector with dimension names.
    #' This field cannot be assigned.
    dimension_names = function(value) {
      if (missing(value)) {
        colnames(private$locations_df)
      } else {
        stop("Cannot set number of dimensions (read-only).")
      }
    },

    #' @field samples
    #' Samples in the batch container.
    #' When assigning data.frame should not have column named .sample_id column.
    samples = function(samples) {
      if (missing(samples)) {
        private$samples_table
      } else {
        assertthat::assert_that(!is.null(samples),
          msg = "samples argument is NULL"
        )

        assertthat::assert_that(is.null(private$samples_table),
          msg = "batch container already has samples"
        )

        validate_samples(samples)

        assertthat::assert_that(nrow(samples) <= self$n_locations,
          msg = "more samples than availble locations in the batch container"
        )

        assertthat::assert_that(nrow(samples) > 0 && ncol(samples) > 0,
          msg = "samples should be a non-empty data.frame"
        )

        assertthat::assert_that(length(intersect(self$dimension_names, colnames(samples))) == 0,
          msg = "some of the samples columns match batch container dimension names"
        )

        assertthat::assert_that(!".sample_id" %in% colnames(samples),
          msg = "samples data.frame has a column with reserved name .sample_id"
        )

        samples$.sample_id <- seq_len(nrow(samples))

        private$samples_table <- dplyr::select(
          samples, .sample_id, dplyr::everything()
        )

        private$samples_dt_cache <- NULL
      }
    },

    #' @field samples_attr
    #' Extra attributes of samples. If set, this is included into
    #' `BatchContainer$get_samples()` output.
    samples_attr = function(sattr) {
      if (missing(sattr)) {
        tibble::as_tibble(private$samples_attributes)
      } else {
        if (!is.null(sattr)) {
          assertthat::assert_that(
            is.data.frame(sattr),
            ncol(sattr) >= 1,
            msg = "samples_attr should be a non-empty table"
          )

          assertthat::assert_that(
            nrow(sattr) == nrow(private$samples_table),
            msg = "samples_attr number of rows should match samples"
          )

          assertthat::assert_that(length(intersect(self$dimension_names, colnames(sattr))) == 0,
            msg = "some of the samples attr columns match batch container dimension names"
          )

          assertthat::assert_that(length(intersect(colnames(private$samples_table), colnames(sattr))) == 0,
            msg = "some of the samples attr columns match samples table column names"
          )

          assertthat::assert_that(!".sample_id" %in% colnames(sattr),
            msg = "samples data.frame has a column with reserved name .sample_id"
          )
        }
        private$samples_attributes <- data.table::as.data.table(sattr)
      }
    },

    #' @field assignment
    #' Sample assignment vector. Should contain NAs for empty locations.
    #'
    #' Assigning this field is deprecated, please use `$move_samples()` instead.
    assignment = function(assignment) {
      if (missing(assignment)) {
        return(private$assignment_vector)
      } else {
        warning("this field might become read-only in the future, please use $move_samples() instead")
        private$validate_assignment(assignment)
        private$assignment_vector <- assignment
        private$samples_dt_cache <- NULL
      }
    }
  ),
  cloneable = FALSE
)
