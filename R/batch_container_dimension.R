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
