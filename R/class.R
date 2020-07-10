#' Dummy distribution function which distributes samples randomly.
#'
#' @export
#' @param samples data.frame with samples and unique .sample_id field.
#' @param batch_container Instance of BatchContainer class
distribute_random <- function(samples, batch_container) {
  assertthat::assert_that(assertthat::has_name(samples, '.sample_id'))

  locations <- batch_container$locations_df

  assertthat::assert_that(nrow(locations) >= nrow(samples))

  n_samples <- nrow(samples)

  expanded_ids <- c(samples$.sample_id,
                    rep(NA_integer_, nrow(locations) - nrow(samples)))

  locations$.sample_id <- sample(expanded_ids)

  samples <- samples %>%
    dplyr::left_join(locations, by='.sample_id') %>%
    dplyr::select(-.sample_id)

  assertthat::assert_that(n_samples == nrow(samples))

  return(samples)
}

#' General function for performing sample assignment.
#'
#' @export
#' @param samples data.frame with samples and unique .sample_id field.
#' @param batch_container Instance of BatchContainer class
#' @param sitribution_function Function performing sample distribution
#' @param random_seed If set will fix random seed and then return the original value
distribute_samples <- function(samples, batch_container,
                               distribution_function=distribute_random,
                               random_seed=NULL) {
  validate_samples(samples)

  assertthat::assert_that(nrow(samples) <= batch_container$n_available,
                          msg="More samples than availble locations in the batch container")

  assertthat::assert_that(length(intersect(batch_container$dimension_names, colnames(samples))) == 0,
                          msg="Some of the samples columns match batch container dimension names")

  assertthat::assert_that(!'.sample_id' %in% colnames(samples),
                          msg="Samples data.frame has a column with reserved name .sample_id")

  assertthat::assert_that(inherits(batch_container, "BatchContainer"),
                          msg="batch_container should be an instance of the BatchContainer class")

  if (!is.null(random_seed)) {
    if (!exists(".Random.seed"))
      set.seed(NULL)
    saved_seed <- .Random.seed
    on.exit({.Random.seed <<- saved_seed})
    set.seed(random_seed)
  } else {
    saved_seed <- NULL
  }

  samples$.sample_id <- 1:nrow(samples)

  samples <- distribution_function(samples, batch_container)

  return(samples)
}

#' Validates sample data.frame.
validate_samples <- function(samples) {
  assertthat::assert_that(is.data.frame(samples),
                          msg="Samples should be a data.frame or tibble")

  assertthat::assert_that(nrow(samples) == dplyr::n_distinct(samples),
                          msg="Non-unique rows in samples")

  assertthat::assert_that(nrow(samples) >=1,
                          msg="Samples should have at least one row")
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
    #' by the sample distribution function.
    weight = NULL,

    #' @field parent_dimension name of the parent dimension.
    #' E.g., plate rows and columns belong to the plate.
    #' However, there is no nesting between rows and columns.
    #' This could be used by a sample distribution function
    #' taking plate effect into account.
    parent_dimension = NULL,

    initialize = function(
                          name,
                          size = NULL,
                          values = NULL,
                          weight = NA_real_,
                          parent_dimension = NULL) {
      assertthat::assert_that(assertthat::is.string(name), name != "",
                              msg="Dimension name should a non-empty string")

      assertthat::assert_that(name != '.sample_id',
                              msg="Cannot use reserved name for a dimension (.sample_id)")
      self$name <- name

      assertthat::assert_that(!is.null(size) || !is.null(values),
                              msg="You need to provide values or size for a dimension")

      if (!is.null(size)) {
        assertthat::assert_that(assertthat::is.count(size), size >= 1,
                                msg="Dimension size should be a positive integer")

        self$values <- 1:size
      } else {
        assertthat::assert_that(is.numeric(values) ||
                                is.character(values) ||
                                is.factor(values),
                                msg='values should be numeric, factor or character vector')
        assertthat::assert_that(length(values) > 0)

        if (is.numeric(values)) {
          assertthat::assert_that(all(values %% 1 == 0),
                                  msg='numeric values are suppose to be integer')

          values <- as.integer(values)
        }

        if (is.factor(values)) {
          values <- levels(x)[levels(x) %in% x]
          assertthat::assert_that(is.character(values), length(values) > 0)
        }

        assertthat::assert_that(all(!duplicated(values)), msg='values are duplicated')

        self$values <- values
      }

      assertthat::assert_that(length(weight) == 1)

      if (!is.na(weight))
        assertthat::assert_that(is.numeric(weight), is.finite(weight), weight >=0)

      self$weight <- weight

      if (!is.null(parent_dimension))
        assertthat::assert_that(assertthat::is.string(parent_dimension),
                                parent_dimension != "")
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
#' @export
BatchContainer <- R6::R6Class("BatchContainer",
  public = list(
    initialize = function(
                          dimensions,
                          interactions = FALSE,
                          interaction_weights = NULL,
                          exclude = NULL) {
      assertthat::assert_that(length(dimensions) >= 1)

      assertthat::assert_that(!is.null(names(dimensions)),
                              msg='Dimensions should have names')

      private$dimensions <- purrr::imap(dimensions, function(dm, name) {
        if (is.numeric(dm)) {
          assertthat::assert_that(!is.null(name), msg="dimension should have a name")
          BatchContainerDimension$new(name = name, size = dm)
        } else if (inherits(dm, "BatchContainerDimension")) {
          assertthat::assert_that(dm$name == name,
                                  'Dimension names should match the list names')
          dm
        } else {
          assertthat::assert_that(is.list(dm),
                                  msg="Dimensions should be named numeric vector, list of BatchContainerDimension or list of lists/integers")
          assertthat::assert_that(is.null(dm$name) || dm$name==name,
                                  msg="dimension name should be set as a list name")
          dm$name <- name
          do.call(BatchContainerDimension$new, dm)
        }
      })

      assertthat::assert_that(all(!duplicated(self$dimension_names)),
                              msg='duplicated dimension names')

      self$exclude <- exclude
    },

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
    exclude_df = NULL
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
    n_dimension= function(value) {
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
        private$dimensions %>%
          purrr::map(~ .x$values) %>%
          expand.grid() %>%
          dplyr::arrange(dplyr::across(dplyr::everything())) %>%
          tibble::as_tibble() %>%
          dplyr::anti_join(self$exclude, by=self$dimension_names)
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

        assertthat::assert_that(is.data.frame(value), msg="Exclude should be a data.frame/tibble or NULL")

        assertthat::assert_that(setequal(colnames(value), names(private$dimensions)),
                                         msg="Columns of exclude should match dimensions")

        value <- value[, names(private$dimensions)] %>%
          dplyr::mutate(dplyr::across(where(is.numeric), as.integer),
                        dplyr::across(where(is.factor), as.character))

        rownames(value) <- NULL

        assertthat::assert_that(nrow(value) == dplyr::n_distinct(value),
                                msg="non-unique rows in exclude")

        assertthat::assert_that(nrow(value) < self$n_locations,
                                msg="All locations in a container cannot be excluded")

        for (dim_name in names(private$dimensions)) {
          assertthat::assert_that(
            all(value[[dim_name]] %in% private$dimensions[[dim_name]]$values),
            msg=stringr::str_glue("Some values are outside range in dimension '{dim_name}'")
            )
        }

        private$exclude_df <- tibble::as_tibble(value)
      }
    }
  )
)
