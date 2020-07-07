Design <- R6::R6Class("Design",
  public = list(
    samples = NULL,
    batches = NULL,
    seed = NULL,
    trace = NULL,
    criterion = NULL,

    new_batch_container = function(
      # arguments
      plate,
      row,
      column,
      interactions=FALSE,
      exclude=NULL,
    ) {
      # make new batch container
      self$batches <- tibble::tibble(
        Batch = 'Batch1'
      )
    },

    distribute_samples = function(seed = NULL) {
      self$seed <- seed
      private$random_seed <- .Random.seed
      private$validate_samples(self$samples)
      randomize(self$batches, self$samples)
    }
  ),

  private = list(
    random_seed = NULL,

    validate_samples = function(x) {
      # stopifnot(is_good(x))  # assertthat::assert_that()
    }
  )
)

distribute_samples <- function(samples, batch_container) {
  validate_samples(samples)

  if (nrow(samples) > batch_container$n_available)
    stop("More samples than availble elements in the batch container")


}

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
BatchContainerDimension <- R6::R6Class("BatchContainerDimension",
  public = list(
    name = NULL,
    size = NULL,
    weight = NULL,
    parent_dimension = NULL,

    initialize = function(
                          name,
                          size,
                          weight = NA,
                          parent_dimension = NULL) {
      if (!is.character(name) || length(name) != 1 || name == "") {
        stop("dimension name should a non-empty character of length 1")
      }

      if (!is.numeric(size) || length(size) != 1 || size < 1 || size %% 1 != 0) {
        stop("dimension size should be a positive integer")
      }

      if (!is.na(weight) &&
        (!is.numeric(weight) ||
          length(weight) != 1 ||
          weight < 0 ||
          is.infinite(weight))) {
        stop("weight should be a finate non-negative numeric of length 1")
      }

      if (!is.null(parent_dimension) &&
        (!is.character(parent_dimension) ||
          length(parent_dimension) != 1 ||
          parent_dimension == "")) {
        stop("parent_dimension should be a non-empty character")
      }

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
    }
  )
)


#' R6 Class representing a batch container.
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

      private$dimensions <- purrr::imap(dimensions, function(element, name) {
        if (is.numeric(element)) {
          BatchContainerDimension$new(name = name, size = element)
        } else if (class(element)[1] == "BatchContainerDimension") {
          element
        } else {
          BatchContainerDimension$new(
            name = name, size = element$size,
            weight = element$weight,
            parent_dimension = element$parent_dimension
          )
        }
      })

      self$exclude <- exclude
    },

    print = function(...) {
      cat(stringr::str_glue(
        "Batch container with {self$n_elements} elements and {self$n_excluded} excluded.\n",
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
    n_elements = function(value) {
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
        self$n_elements - self$n_excluded
      } else {
        stop("Cannot set number of elements in container (read-only).")
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

        if (nrow(value) >= self$n_elements) {
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
