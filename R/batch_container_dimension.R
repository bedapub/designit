#' R6 Class representing a batch container dimension.
#'
#' @export
BatchContainerDimension <- R6::R6Class("BatchContainerDimension",
  public = list(
    #' @field name dimension name.
    name = NULL,

    #' @field values vector of dimension values.
    values = NULL,

    #' @description
    #' Create a new BatchContainerDimension object.
    #'
    #' This is usually used implicitly via [`BatchContainer$new()`][BatchContainer].
    #'
    #' @param name Dimension name, a character string. Requiered.
    #' @param size Dimension size. Setting this implies that dimension values are 1:`size`.
    #' @param values Explicit list of dimension values. Could be numeric, character or factor.
    #'
    #' It is required to provide dimension namd and either size of values.
    #'
    #' @examples
    #' plate_dimension <- BatchContainerDimension$new("plate", size=3)
    #' row_dimension <- BatchContainerDimension$new("row", values = letters[1:3])
    #' column_dimension <- BatchContainerDimension$new("column", values = 1:3)
    #'
    #' bc <- BatchContainer$new(
    #'   dimensions = list(plate_dimension, row_dimension, column_dimension),
    #'   exclude = data.frame(plate = 1, row = "a", column = c(1, 3), stringsAsFactors = FALSE)
    #' )
    #'
    #' bc
    initialize = function(name,
                          size = NULL,
                          values = NULL) {
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

        self$values <- seq_len(size)
      } else {
        assertthat::assert_that(is.numeric(values) ||
          is.character(values) ||
          is.factor(values),
        msg = "values should be numeric, factor or character vector"
        )
        assertthat::assert_that(!any(is.na(values)),
          msg = "NA values are not allowed"
        )
        assertthat::assert_that(length(values) > 0)

        if (is.numeric(values)) {
          assertthat::assert_that(all(values %% 1 == 0),
            msg = "numeric values are suppose to be integer"
          )

          values <- as.integer(values)
        }

        if (is.factor(values)) {
          values <- levels(values)[levels(values) %in% values]
          assertthat::assert_that(is.character(values), length(values) > 0)
        }

        assertthat::assert_that(all(!duplicated(values)), msg = "values are duplicated")

        self$values <- values
      }
    }
  ),
  active = list(
    #' @field size
    #' Returns size of a dimension.
    size = function(value) {
      if (missing(value)) {
        length(self$values)
      } else {
        stop("size is a read-only field")
      }
    },
    #' @field short_info
    #' Returns a string summarizing the dimension.
    #' E.g., "mydim<size=10>".
    short_info = function(value) {
      if (missing(value)) {
        stringr::str_glue("{self$name}<size={self$size}>")
      } else {
        stop("short_info is a read-only field")
      }
    }
  )
)
