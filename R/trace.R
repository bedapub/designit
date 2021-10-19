#' OptimizationTrace represents optimization trace.
#' Usually it is created by [optimize_design()].
OptimizationTrace <- R6::R6Class("OptimizationTrace",
  public = list(
    #' @field scores
    #' Contains a matrix of scores. The matrix size is usually
    #' `c(iterations + 1, length(bc$scoring_f))`
    scores = NULL,

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
    #' Number of values to save. Usually `n_steps == iterations + `.
    #' @param n_scores
    #' Number of scoring functions.
    #' @param score_names
    #' Names of scoring functions.
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
    #'
    #' @return `OptimizationTrace` invisibly.
    set_scores = function(i, scores) {
      self$scores[i, ] <- scores
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
      start_score <- self$scores[1, 1]
      final_score <- self$scores[nrow(self$scores), 1]
      cat(stringr::str_glue("Optimization trace ({self$n_steps} score values, elapsed {format(self$elapsed)}).\n\n"))
      cat("  Starting score: ", start_score, "\n", sep = "")
      cat("  Final score   : ", final_score, "\n", sep = "")
      invisible(self)
    },

    #' @description
    #' Plot `OptimizationTrace`. Only the main score at the moment.
    #'
    #' @param ...
    #' Extra arguments passed to [ggplot2::qplot()].
    plot = function(...) {
      ggplot2::qplot(
        x = seq_len(nrow(self$scores)), y = self$scores[, 1],
        geom = c("point", "line"),
        xlab = "step", ylab = "score",
        ...
      )
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
