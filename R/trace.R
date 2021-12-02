#' OptimizationTrace represents optimization trace.
#' Usually it is created by [optimize_design()].
OptimizationTrace <- R6::R6Class("OptimizationTrace",
  public = list(
    #' @field scores
    #' Contains a matrix of scores. The matrix size is usually
    #' `c(iterations + 1, length(bc$scoring_f))`
    scores = NULL,

    #' @field aggregated_score
    #' Contains a vector of aggregated scores. The vector length is usually
    #' `c(iterations + 1)`
    aggregated_score = NULL,

    #' @field seed
    #' Saved value of [.Random.seed].
    seed = NULL,

    #' @field elapsed
    #' Running time of the optimization.
    elapsed = NULL,

    #' @description
    #' Create a new `OptimizationTrace` object.
    #'
    #' @param n_steps
    #' Number of values to save. Usually `n_steps == iterations + 1`.
    #' @param n_scores
    #' Number of scoring functions.
    #' @param score_names
    #' Names of scoring functions.
    initialize = function(n_steps, n_scores, score_names) {
      self$scores <- matrix(NA_real_, nrow = n_steps, ncol = n_scores)
      self$aggregated_score <- rep(NA_real_, n_steps)
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
    #' @param aggregated_score
    #' Aggregated score, a double value.
    #'
    #' @return `OptimizationTrace` invisibly.
    set_scores = function(i, scores, aggregated_score) {
      self$scores[i, ] <- scores
      self$aggregated_score[i] <- aggregated_score
      invisible(self)
    },

    #' @description
    #' Shrink scores by keeping only first `last_step` scores.
    #'
    #' @param last_step
    #' Last step to keep.
    #'
    #' @return `OptimizationTrace` invisibly.
    shrink = function(last_step) {
      self$scores <- head(self$scores, last_step)
      self$aggregated_score <- head(self$aggregated_score, last_step)
      invisible(self)
    },

    #' @description
    #' Print `OptimizationTrace`.
    #'
    #' @param ...
    #' Unused.
    #'
    #' @return `OptimizationTrace` invisibly.
    print = function(...) {
      start_score <- self$aggregated_score[1]
      final_score <- self$aggregated_score[length(self$aggregated_score)]
      cat(stringr::str_glue("Optimization trace ({self$n_steps} score values, elapsed {format(self$elapsed)}).\n\n"))
      cat("  Starting score: ", start_score, "\n", sep = "")
      cat("  Final score   : ", final_score, "\n", sep = "")
      invisible(self)
    },

    #' @description
    #' Plot `OptimizationTrace`. Only the main score at the moment.
    #'
    #' @param aggregated_only only plot the aggregated score
    #' @param ...
    #' Extra arguments passed to [ggplot2::ggplot()].
    plot = function(aggregated_only = FALSE, ...) {

      plot_data <- data.frame(Iteration = 1:length(self$aggregated_score),
                              AggregatedScore = self$aggregated_score)
      if(!aggregated_only && (ncol(self$scores) > 1)) {
        plot_data <- cbind(plot_data, self$scores)
      }

      tidyr::pivot_longer(plot_data, names_to = "ScoreName", values_to = "Score",
                          cols = -Iteration) %>%
        ggplot2::ggplot(aes(x = Iteration, y = Score)) +
        geom_point() + geom_line() +
          facet_wrap(~ScoreName, scales = "free_y")
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
