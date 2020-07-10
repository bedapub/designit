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

  if (nrow(samples) > batch_container$n_available)
    stop("More samples than availble elements in the batch container")

  if (length(intersect(batch_container$dimension_names, colnames(samples))) > 0)
    stop("Some of the samples columns match batch container dimension names")

  if ('.sample_id' %in% colnames(samples))
    stop("Samples data.frame has a column with reserved name .sample_id")

  if (!inherits(batch_container, "BatchContainer"))
    stop("batch_container should be an instance of the BatchContainer class")

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
  if (!is.data.frame(samples))
    stop("Samples should be a data.frame or tibble")

  if (nrow(samples) != dplyr::n_distinct(samples)) {
    stop("Non-unique rows in samples")
  }

  if (nrow(samples) < 1) {
    stop("Samples should have at least one row")
  }
}



#' R6 Class representing a batch container dimension.
#'
#' @export
BatchContainerDimension <- R6::R6Class("BatchContainerDimension",
  public = list(

    #' @field name dimension name.
    name = NULL,

    #' @field size dimension size.
    size = NULL,

    #' @field weight dimension weight. This can be interpreted
    #' by the sample distribution function.
    weight = NULL,

    #' @field parent_dimension parent of the dimension.
    #' E.g., plate rows and columns belong to the plate.
    #' However, there is no nesting between rows and columns.
    #' This could be used by a sample distribution function
    #' taking plate effect into account.
    parent_dimension = NULL,

    initialize = function(
                          name,
                          size,
                          weight = NA,
                          parent_dimension = NULL) {
      if (!is.character(name) || length(name) != 1 || name == "")
        stop("Dimension name should a non-empty character of length 1")

      if (name == '.sample_id')
        stop("Cannot use reserved name for a dimension (.sample_id)")

      if (!is.numeric(size) || length(size) != 1 || size < 1 || size %% 1 != 0)
        stop("Dimension size should be a positive integer")

      if (!is.na(weight) &&
        (!is.numeric(weight) ||
          length(weight) != 1 ||
          weight < 0 ||
          is.infinite(weight)))
        stop("weight should be a finate non-negative numeric of length 1")

      if (!is.null(parent_dimension) &&
        (!is.character(parent_dimension) ||
          length(parent_dimension) != 1 ||
          parent_dimension == ""))
        stop("parent_dimension should be a non-empty character")

      self$name <- name
      self$size <- as.integer(size)
      self$weight <- weight
      self$parent_dimension <- parent_dimension
    }
  ),

  active = list(
    short_info = function(value) {
      if (missing(value)) {
        stringr::str_glue("{self$name}<size={self$size}>")
      } else {
        stop("short_info is a read-only field")
      }
    },
    values = function(value) {
      if (missing(value)) {
        res <- list(1:self$size)
        names(res) <- self$name
        res
      } else {
        stop("values is a read-only field")
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
                          dimensions = NULL,
                          interactions = FALSE,
                          interaction_weights = NULL,
                          exclude = NULL) {
      if (is.null(dimensions) || length(dimensions) == 0) {
        stop("dimensions cannot be NULL or 0 length")
      }

      if (is.null(names(dimensions))) {
        stop("dimensions should have names")
      }

      if (any(duplicated(names(dimensions)))) {
        stop("dimensions must have unique names")
      }

      private$dimensions <- purrr::imap(dimensions, function(dm, name) {
        if (is.numeric(dm)) {
          BatchContainerDimension$new(name = name, size = dm)
        } else if (inherits(dm, "BatchContainerDimension")) {
          dm
        } else {
          BatchContainerDimension$new(
            name = name, size = dm$size,
            weight = dm$weight,
            parent_dimension = dm$parent_dimension
          )
        }
      })

      self$exclude <- exclude
    },

    print = function(...) {
      cat(stringr::str_glue(
        "Batch container with {self$n_locations} elements and {self$n_excluded} excluded.\n",
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
        stop("Cannot set number of elements in container (read-only).")
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
        stop("Cannot set number of excluded elements in container (read-only).")
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
          purrr::flatten() %>%
          expand.grid() %>%
          tibble::as_tibble() %>%
          dplyr::anti_join(self$exclude, by=self$dimension_names)
      } else {
        stop("Cannot set elements data.frame (read-only).")
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

        if (!is.data.frame(value)) {
          stop("Exclude should be a data.frame/tibble or NULL")
        }

        if (!setequal(colnames(value), names(private$dimensions))) {
          stop("Columns of exclude should match dimensions")
        }

        value <- value[, names(private$dimensions)] %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))

        rownames(value) <- NULL

        if (nrow(value) != dplyr::n_distinct(value)) {
          stop("non-unique rows in exclude")
        }

        if (nrow(value) >= self$n_locations) {
          stop("All the elements of the container cannot be excluded")
        }

        for (dim_name in names(private$dimensions)) {
          if (any(value[[dim_name]] > private$dimensions[[dim_name]]$size |
            value[[dim_name]] < 1)) {
            stop(stringr::str_glue("Some values are outside range in dimension '{dim_name}'"))
          }
        }

        private$exclude_df <- tibble::as_tibble(value)
      }
    }
  )
)
