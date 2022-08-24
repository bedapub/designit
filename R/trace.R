#' OptimizationTrace represents optimization trace.
#' Usually it is created by [optimize_design()].
#'
#' @export
OptimizationTrace <- R6::R6Class("OptimizationTrace",
  public = list(
    #' @field scores
    #' Contains a matrix of scores. The matrix size is usually
    #' `c(iterations + 1, length(bc$scoring_f))`
    scores = NULL,

    #' @field aggregated_scores
    #' Contains a matrix of scores after aggregation.
    #' The matrix size is usually `c(iterations + 1, length(aggregated))`,
    #' where `length(aggregated)` is the length of aggregated scores vector.
    #' Can be `NULL` if aggregated scores are not used.
    aggregated_scores = NULL,

    #' @field seed
    #' Saved value of [.Random.seed].
    seed = NULL,

    #' @field elapsed
    #' Running time of the optimization.
    elapsed = NULL,

    #' @field last_step
    #' Last iteration step for which the score was set.
    last_step = 0,

    #' @description
    #' Create a new `OptimizationTrace` object.
    #'
    #' @param n_steps
    #' Number of values to save. Usually `n_steps == iterations + 1`.
    #' @param n_scores
    #' Number of scoring functions.
    #' @param score_names
    #' Names of scoring functions.
    #' @example man/examples/trace.R
    initialize = function(n_steps, n_scores, score_names) {
      self$scores <- matrix(NA_real_, nrow = n_steps, ncol = n_scores)
      if (!is.null(score_names)) {
        dimnames(self$scores) <- list(NULL, score_names)
      }
    },

    #' @description
    #' Set scores for i-th step.
    #'
    #' @param i
    #' Step number.
    #' @param scores
    #' Scores, a vector or a value if no auxiliary functions are used.
    #' @param aggregated_scores
    #' Vector of aggregated scores. Can be NULL.
    #'
    #' @return `OptimizationTrace` invisibly.
    #' @example man/examples/trace.R
    set_scores = function(i, scores, aggregated_scores) {
      assertthat::assert_that(
        assertthat::is.count(i),
        is.vector(scores),
        is.null(aggregated_scores) || is.vector(aggregated_scores)
      )
      # initialize aggregated scores, in case they're empty
      self$scores[i, ] <- scores
      if (!is.null(aggregated_scores)) {
        if (is.null(self$aggregated_scores)) {
          self$aggregated_scores <- matrix(
            NA_real_,
            nrow = nrow(self$scores),
            ncol = length(aggregated_scores)
          )
        }
        assertthat::assert_that(
          length(aggregated_scores) == ncol(self$aggregated_scores)
        )
        self$aggregated_scores[i, ] <- aggregated_scores
      }
      self$last_step <- i
      invisible(self)
    },

    #' @description
    #' Shrink scores by keeping only first `last_step` scores.
    #'
    #' @param last_step
    #' Last step to keep.
    #'
    #' @return `OptimizationTrace` invisibly.
    #' @example man/examples/trace.R
    shrink = function(last_step = self$last_step) {
      self$scores <- head(self$scores, last_step)
      if (!is.null(self$aggregated_scores)) {
        self$aggregated_scores <- head(self$aggregated_scores, last_step)
      }
      invisible(self)
    },

    #' @description
    #' Return individual (not aggregated!) scores by keeping only first `last_step` scores.
    #'
    #' @param last_step
    #' Last step to keep.
    #'
    #' @return `OptimizationTrace` invisibly.
    get_scores = function(last_step = self$last_step) {
      head(self$scores, last_step)
    },

    #' @description
    #' Print `OptimizationTrace`.
    #'
    #' @param ...
    #' Unused.
    #'
    #' @return `OptimizationTrace` invisibly.
    print = function(...) {
      start_score <- self$scores[1, ] %>%
        round(3) %>%
        stringr::str_c(collapse = ",")
      final_score <- tail(self$scores, 1) %>%
        round(3) %>%
        stringr::str_c(collapse = ",")
      cat(stringr::str_glue("Optimization trace ({self$n_steps} score values, elapsed {format(self$elapsed)}).\n\n"))
      cat("  Starting score: ", start_score, "\n", sep = "")
      cat("  Final score   : ", final_score, "\n", sep = "")
      invisible(self)
    },

    #' @description
    #' Convert to a [data.frame].
    #'
    #' @param include_aggregated Include aggregated scores. Otherwise only
    #' raw scores are exported.
    #'
    #' @return [data.frame]
    as_tibble = function(include_aggregated = TRUE) {
      scores <- make_colnames(self$scores, "score") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(step = dplyr::row_number()) %>%
        tidyr::pivot_longer(
          c(-step),
          names_to = "score",
          values_to = "value"
        ) %>%
        dplyr::mutate(score = factor(score))
      if (include_aggregated) {
        agg_scores <- self$aggregated_scores
      } else {
        agg_scores <- NULL
      }
      if (!is.null(agg_scores) && include_aggregated) {
        colnames(agg_scores) <- stringr::str_c(
          "agg.", seq_len(ncol(agg_scores))
        )
        agg_scores <- agg_scores %>%
          tibble::as_tibble() %>%
          dplyr::mutate(step = dplyr::row_number()) %>%
          tidyr::pivot_longer(
            c(-step),
            names_to = "score",
            values_to = "value"
          ) %>%
          dplyr::mutate(score = factor(score))
      }
      dplyr::bind_rows(
        score = scores,
        aggregated = agg_scores,
        .id = "type"
      ) %>%
        dplyr::mutate(type = factor(type, levels = c("score", "aggregated")))
    },

    #' @description
    #' Plot `OptimizationTrace`. Only the main score at the moment.
    #'
    #' @param include_aggregated Include aggregated scores. Otherwise only
    #' raw scores are plotted.
    #' @param ...
    #' Not used.
    #' @examples
    #' tr <- OptimizationTrace$new(10, 3, letters[1:3])
    #' for (i in seq_len(10)) {
    #'   tr$set_scores(i, rnorm(3)*(1:3), rnorm(3)*(1:3))
    #' }
    #'
    #' # plot only the main scores
    #' plot(tr)
    #' # plot main and aggregated scores
    #' plot(tr, include_aggregated=TRUE)
    plot = function(include_aggregated = FALSE, ...) {
      p <- self$as_tibble(include_aggregated = include_aggregated) %>%
        ggplot2::ggplot() +
        ggplot2::aes(x = step, y = value, group = score, color = score) +
        ggplot2::geom_point() +
        ggplot2::geom_line()

      if (include_aggregated) {
        p +
          ggplot2::facet_wrap(~type, scales = "free_y", ncol = 1)
      } else {
        p +
          ggplot2::facet_wrap(~score, scales = "free_y", ncol = 1)
      }
    }
  ),
  active = list(
    #' @field n_steps
    #' Returns number of steps in the `OptimizationTrace`.
    n_steps = function(value) {
      if (missing(value)) {
        nrow(self$scores)
      } else {
        stop("Cannot set n_steps (read-only).")
      }
    }
  )
)

#' Make [matrix] column names unique.
#'
#' @param prefix Prefix to add if column names are empty.
#' @return A [matrix] with updated column names.
#'
#' @keywords internal
make_colnames <- function(m, prefix = "X") {
  if (is.null(colnames(m))) {
    colnames(m) <- rep("", ncol(m))
  }
  if (all(colnames(m) == "")) {
    colnames(m) <- rep(prefix, ncol(m))
  }
  colnames(m) <- make.names(colnames(m), unique = TRUE)
  m
}
