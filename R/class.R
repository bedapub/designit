#' Dummy assignment function which distributes samples randomly.
#'
#' @export
#' @param samples data.frame with samples and unique .sample_id field.
#' @param batch_container Instance of BatchContainer class
assign_random <- function(batch_container, samples = NULL) {
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples(),
      msg = "batch-container is empty and no samples provided"
    )
  } else {
    batch_container$samples_df <- samples
  }

  n_samples <- nrow(batch_container$samples_df)
  n_available <- batch_container$n_available

  assertthat::assert_that(n_available >= n_samples)

  expanded_ids <- c(
    1:n_samples,
    rep(NA_integer_, n_available - n_samples)
  )

  batch_container$assignment_vec <- sample(expanded_ids)

  invisible(batch_container)
}

#' Validates sample data.frame.
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
}



#' R6 Class representing a batch container dimension.
#'
#' @export
BatchContainerDimension <- R6::R6Class("BatchContainerDimension",
  public = list(

    #' @field name dimension name.
    name = NULL,

    #' @field values vector of dimension values.
    values = NULL,

    #' @field weight dimension weight. This can be interpreted
    #' by the sample assignment function.
    weight = NULL,

    #' @field parent_dimension name of the parent dimension.
    #' E.g., plate rows and columns belong to the plate.
    #' However, there is no nesting between rows and columns.
    #' This could be used by a sample assignment function
    #' taking plate effect into account.
    parent_dimension = NULL,

    initialize = function(
                          name,
                          size = NULL,
                          values = NULL,
                          weight = NA_real_,
                          parent_dimension = NULL) {
      assertthat::assert_that(assertthat::is.string(name), name != "",
        msg = "Dimension name should a non-empty string"
      )

      assertthat::assert_that(name != ".sample_id",
        msg = "Cannot use reserved name for a dimension (.sample_id)"
      )
      self$name <- name

      assertthat::assert_that(!is.null(size) || !is.null(values),
        msg = "You need to provide values or size for a dimension"
      )

      if (!is.null(size)) {
        assertthat::assert_that(assertthat::is.count(size), size >= 1,
          msg = "Dimension size should be a positive integer"
        )

        self$values <- 1:size
      } else {
        assertthat::assert_that(is.numeric(values) ||
          is.character(values) ||
          is.factor(values),
        msg = "values should be numeric, factor or character vector"
        )
        assertthat::assert_that(length(values) > 0)

        if (is.numeric(values)) {
          assertthat::assert_that(all(values %% 1 == 0),
            msg = "numeric values are suppose to be integer"
          )

          values <- as.integer(values)
        }

        if (is.factor(values)) {
          values <- levels(x)[levels(x) %in% x]
          assertthat::assert_that(is.character(values), length(values) > 0)
        }

        assertthat::assert_that(all(!duplicated(values)), msg = "values are duplicated")

        self$values <- values
      }

      assertthat::assert_that(length(weight) == 1)

      if (!is.na(weight)) {
        assertthat::assert_that(is.numeric(weight), is.finite(weight), weight >= 0)
      }

      self$weight <- weight

      if (!is.null(parent_dimension)) {
        assertthat::assert_that(
          assertthat::is.string(parent_dimension),
          parent_dimension != ""
        )
      }
      self$parent_dimension <- parent_dimension
    }
  ),

  active = list(
    size = function(value) {
      if (missing(value)) {
        length(self$values)
      } else {
        stop("size is a read-only field")
      }
    },
    short_info = function(value) {
      if (missing(value)) {
        stringr::str_glue("{self$name}<size={self$size}>")
      } else {
        stop("short_info is a read-only field")
      }
    }
  )
)


#' R6 Class representing a batch container.
#'
#' @description
#' Describes container dimensions and samples to container location assignment.
#'
#' @details
#' A typical workflow starts with creating a \code{BatchContainer}. Then
#' samples can be assigned to locations in that container.
#'
#' @export

BatchContainer <- R6::R6Class("BatchContainer",
  public = list(
    #' @field scoring_f
    #' Main scoring function used for optimization.
    scoring_f = NULL,

    #' @field aux_scoring_f
    #' Additional scoring functions to compute
    aux_scoring_f = NULL,

    #' @description
    #' Create a new BatchContainer object.
    #' @param dimensions A vector or list of dimensions. Every dimension
    #' should have a name. Could be an integer vector of dimensions or
    #' a named list. Every value of a list could be either dimension size
    #' or parameters for \code{BatchContainerDimension$new()}.
    #' @param interactions deprecated
    #' @param interaction_weights deprecated
    #' @param exclude \code{data.frame} with excluded locations of a container.
    #' @examples
    #' bc <- BatchContainer$new(
    #'   dimensions = list(
    #'     "plate" = 3,
    #'     "row" = list(values = letters[1:3]),
    #'     "column" = list(values = c(1, 3))
    #'   ),
    #'   exclude = data.frame(plate = 1, row = "a", column = c(1, 3), stringsAsFactors = F)
    #' )
    #'
    #' bc
    initialize = function(
                          dimensions,
                          interactions = FALSE,
                          interaction_weights = NULL,
                          exclude = NULL) {
      assertthat::assert_that(length(dimensions) >= 1)

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
            "Dimension names should match the list names"
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
    #' Returns TRUE if batch has samples.
    has_samples = function() {
      !is.null(private$samples)
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
    #' @param dst integer vector of destination locations (the same length as \code{src}).
    #' If NULL, generated from \code{src}: \code{src[c(2:length(src), 1)]}.
    exchange_samples = function(src, dst = NULL) {
      assertthat::assert_that(is.integer(src),
        msg = "src should be integer vector"
      )
      if (is.null(dst)) dst <- src[c(2:length(src), 1)]
      assertthat::assert_that(is.integer(dst) && length(src) == length(dst),
        msg = "src and dst should be integer vectors of the same length"
      )
      # ensure private$samples_dt_cache is set
      self$samples_dt
      sid_ind <- match(".sample_id", colnames(private$samples_dt_cache))
      fcols <- colnames(private$samples_dt_cache)[sid_ind:ncol(private$samples_dt_cache)]
      val <- private$samples_dt_cache[src, fcols, with = FALSE]
      private$samples_dt_cache[dst, (fcols) := val]
      if (any(1:nrow(private$samples) != sort(private$samples_dt_cache$.sample_id))) {
        private$samples_dt_cache <- NULL
        stop("Samples lost or duplicated during exchange; check src and dst")
      }
      private$assignment <- private$samples_dt_cache$.sample_id
    },

    #' @description
    #' Score current sample assignment,
    #' @param aux compute auxiliary scoring functions
    #' @return In case `aux` is FALSE returns the value of the main scoring function.
    #' Otherwise a vector of all scoring functions starting from the first one.
    score = function(aux = FALSE) {
      assertthat::assert_that(assertthat::is.flag(aux), msg = "aux should be TRUE or FALSE")
      assertthat::assert_that(!is.null(self$scoring_f),
        msg = "Scoring function needs to be assigned"
      )
      assertthat::assert_that(!is.null(private$samples),
        msg = "No samples in the batch container, cannot compute score"
      )

      samples <- self$samples_dt
      res <- self$scoring_f(samples)
      assertthat::assert_that(assertthat::is.number(res),
        msg = "Scoring function should return a single number"
      )

      if (aux && !is.null(self$aux_scoring_f) && length(self$aux_scoring_f) >= 1) {
        assertthat::assert_that(is.list(self$aux_scoring_f),
          msg = "auxillary scoring functions should be a list"
        )

        aux_res <- purrr::map_dbl(self$aux_scoring_f, ~ .x(samples))
        assertthat::assert_that(is.double(aux_res))
        assertthat::assert_that(length(aux_res) == length(self$aux_scoring_f))

        res <- c(res, aux_res)
      }

      return(res)
    },

    #' @description
    #' Prints information about \code{BatchContainer}.
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
    }
  ),

  private = list(
    dimensions = NULL,
    exclude_df = NULL,
    #' @description
    #' Tibble with sample information and sample ids.
    samples = NULL,
    #' @description
    #' Tibble with sample ids and assignment to batch container locations.
    assignment = NULL,
    #' @description
    #' Cached data.table with samples assignment.
    samples_dt_cache = NULL,
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
    n_locations = function(value) {
      if (missing(value)) {
        private$dimensions %>%
          purrr::map_int(~ .x$size) %>%
          prod()
      } else {
        stop("Cannot set number of locations in a container (read-only).")
      }
    },
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
    n_available = function(value) {
      if (missing(value)) {
        self$n_locations - self$n_excluded
      } else {
        stop("Cannot set dimension names (read-only).")
      }
    },
    n_dimension = function(value) {
      if (missing(value)) {
        length(private$dimensions)
      } else {
        stop("Cannot set number of dimensions (read-only).")
      }
    },
    dimension_names = function(value) {
      if (missing(value)) {
        names(private$dimensions)
      } else {
        stop("Cannot set number of dimensions (read-only).")
      }
    },
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
    exclude = function(value) {
      if (missing(value)) {
        private$exclude_df
      } else {
        if (is.null(value) || nrow(value) == 0) {
          private$exclude_df <- NULL
          return()
        }

        assertthat::assert_that(is.data.frame(value), msg = "Exclude should be a data.frame/tibble or NULL")

        assertthat::assert_that(setequal(colnames(value), names(private$dimensions)),
          msg = "Columns of exclude should match dimensions"
        )

        value <- value[, names(private$dimensions)] %>%
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

    #' @description
    #' Samples in the batch container.
    #' When assigning data.frame should not have column named .sample_id column.
    samples_df = function(samples) {
      if (missing(samples)) {
        private$samples
      } else {
        assertthat::assert_that(!is.null(samples) || !is.null(private$samples),
          msg = "samples argument is NULL"
        )

        assertthat::assert_that(is.null(private$samples),
          msg = stringr::str_c("batch container already has samples")
        )

        validate_samples(samples)

        assertthat::assert_that(nrow(samples) <= self$n_available,
          msg = "more samples than availble locations in the batch container"
        )

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

    #' @description
    #' Sample assignment \code{data.table}.
    #' This data.table includes columns for all the \code{BatchContainer}
    #' locations, all the samples and a \code{".sample_id"} column.
    #'
    #' The most efficient way of updating this \code{data.table} is using
    #' the \code{$exchange_samples()} method.
    samples_dt = function(value) {
      if (missing(value)) {
        if (is.null(private$samples_dt_cache)) {
          private$samples_dt_cache <- data.table::data.table(
            self$get_samples(include_id = TRUE)
          )
        }
        private$samples_dt_cache
      } else {
        stop("samples_dt is read-only.")
      }
    },

    #' @description
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
  )
)
