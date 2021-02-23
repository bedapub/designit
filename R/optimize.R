#' Shuffles samples trying to improve the scoring function.
#'
#' In every iteration shuffles several samples in the container.
#' If the `batch_container$score()` worsens, reverts to the previous state.
#'
#' @param batch_container An instance of `BatchContainer`.
#' @param samples A `data.frame` with sample information. Should be `NULL` if the `BatchContainer`
#' already has samples in it.
#' @param n_shuffle Number of times shuffling performed at each iterations.
#' Could be a number or a vector of length `iterations`.
#' In the later case, number of samples to shuffle could be precisely
#' set for every iteration. Please not that `n_shuffle` has a slightly different meaning depending
#' on whether you use `shuffle_proposal` or not. If `shuffle_proposal` is not set, this parameter
#' sets the number of items to shuffle (default 2). When `shuffle_proposal` is used, it will be
#' called `n_shuffle` times at an iteration (default 1).
#' @param shuffle_proposal
#' A function used to propose two or more elements to shuffle in every step.
#' If non-`NULL` a function receives two arguments on every iteration:
#' `bc$samples_dt` and the iteration number. This function should return a list with attributes
#' `src` and `dst` (see [`BatchContainer$exchange_samples()`][BatchContainer]).
#' @param iterations Number of iterations. If not provided set to 1000.
#' @return An [OptimizationTrace] object.
#' @export
assign_score_optimize_shuffle <- function(batch_container, samples = NULL, n_shuffle = NULL, shuffle_proposal = NULL, iterations = NULL) {
  start_time <- Sys.time()
  save_random_seed <- .Random.seed
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
      msg = "batch-container is empty and no samples provided"
    )
  } else {
    assertthat::assert_that(nrow(samples) > 0)
    assign_random(batch_container, samples)
  }


  n_avail <- batch_container$n_available
  min_n_shuffle <- if (is.null(shuffle_proposal)) 2 else 1
  if (is.null(n_shuffle) && is.null(iterations)) {
    message("Number of iterations cannot be inferred; setting to 1000 iterations")
    iterations <- 1000
  }
  if (is.null(n_shuffle)) {
    n_shuffle <- min_n_shuffle
  }
  if (length(n_shuffle) > 1 & is.null(iterations)) {
    iterations <- length(n_shuffle)
  }
  assertthat::assert_that(
    is.numeric(n_shuffle) &&
      (length(n_shuffle) == 1 || length(n_shuffle) == iterations),
    msg = "n_shuffle should be an integer vector of length iteration or a single integer value"
  )
  assertthat::assert_that(
    is.null(shuffle_proposal) || is.function(shuffle_proposal),
    msg = "shuffle_proposal should be a function"
  )

  if (length(n_shuffle) == 1) n_shuffle <- rep(n_shuffle, iterations)

  assertthat::assert_that(all(n_shuffle >= min_n_shuffle),
    msg = stringr::str_glue("n_shuffle values should be at least {min_n_shuffle}")
  )

  assertthat::assert_that(!is.null(batch_container$scoring_f), msg = "no scoring function set for BatchContainer")
  trace <- OptimizationTrace$new(
    iterations + 1,
    length(batch_container$aux_scoring_f),
    names(batch_container$aux_scoring_f)
  )
  current_score <- batch_container$score(aux = TRUE)
  trace$set_scores(1, current_score)
  no_proposal <- FALSE

  for (i in seq_len(iterations)) {
    perm <- seq_len(n_avail)
    if (is.function(shuffle_proposal)) {
      for (j in seq_len(n_shuffle[i])) {
        sh <- shuffle_proposal(batch_container$samples_dt, i)
        assertthat::assert_that(is.list(sh), msg = "Shuffle proposal function should return a list")
        src <- sh$src
        dst <- sh$dst
        if (is.null(src)) {
          if (j == 1) {
            no_proposal <- TRUE
          }
          break
        }
        perm[dst] <- perm[src]
        batch_container$exchange_samples(src, dst)
      }
    } else {
      non_empty_loc <- which(!is.na(batch_container$samples_dt$.sample_id))
      pos1 <- sample(non_empty_loc, 1)
      assertthat::assert_that(length(non_empty_loc) > 0,
        msg = "all locations are empty in BatchContainer"
      )
      pos_rest <- sample(which(seq_len(n_avail) != pos1), n_shuffle[i] - 1)
      src <- c(pos1, pos_rest)
      if (length(src) == 2) {
        # there is only one way to shuffle when there are two locations
        dst <- rev(src)
      } else {
        dst <- sample(src)
      }
      perm[dst] <- perm[src]
      batch_container$exchange_samples(src, dst)
    }

    if (no_proposal) {
      message("no shuffling proposed, stopping the optimization")
      trace$shrink(i)
      break
    }

    non_trivial <- which(perm != seq_along(perm))
    if (length(non_trivial) == 0) {
      # the shuffling is a trivial permutation, go to the next iteration
      trace$set_scores(i + 1, current_score)
      next
    }

    new_score <- batch_container$score(aux = TRUE)
    if (new_score[1] >= current_score[1]) {
      batch_container$exchange_samples(non_trivial, perm[non_trivial])
    } else {
      current_score <- new_score
    }

    trace$set_scores(i + 1, current_score)
  }

  trace$seed <- save_random_seed
  trace$elapsed <- Sys.time() - start_time
  return(trace)
}

#' OptimizationTrace represents optimization trace.
#' Usually it is created by [assign_score_optimize_shuffle()].
OptimizationTrace <- R6::R6Class("OptimizationTrace",
  public = list(
    #' @field scores
    #' Contains a matrix of scores. The matrix size is usually
    #' `c(iterations + 1, 1 + length(bc$aux_scoring_f))`
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
    #' @param n_aux
    #' Number of auxiliary scoring functions.
    #' @param names_aux
    #' Names of auxiliary scroring functions.
    initialize = function(n_steps, n_aux, names_aux) {
      self$scores <- matrix(NA_real_, nrow = n_steps, ncol = n_aux + 1)
      if (!is.null(names_aux)) {
        dimnames(self$scores) <- list(NULL, c("", names_aux))
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
      self$scores <- self$scores[seq_len(last_step), ]
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
      cat(stringr::str_glue("Optimization trace ({self$n_steps}) score values, elapsed {format(self$elapsed)}).\n\n"))
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
