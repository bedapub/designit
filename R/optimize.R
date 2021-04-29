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

  for (i in seq_len(iterations)) {
    perm <- seq_len(n_avail)
    if (is.function(shuffle_proposal)) {
      for (j in seq_len(n_shuffle[i])) {
        sh <- shuffle_proposal(batch_container$samples_dt, i)
        assertthat::assert_that(is.list(sh), msg = "Shuffle proposal function should return a list")
        src <- sh$src
        dst <- sh$dst
        if (is.null(src)) {
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


#' Create function to propose 1 pairwise swap of samples on each call
#'
#' This internal function is wrapped by mk_swapping_function()
#'
#' @param n_samples Total number of samples (i.e. max of permutation index)
#'
#' @return Parameter-less function to return a list with length 1 vectors 'src' and 'dst', denoting source and destination index for the swap operation
#'
#' @keywords internal
mk_pairwise_swapping_function = function(n_samples) {
  # Function factory for creator of a 'neighboring' sample arrangement with just one pairwise swap

  n = round(n_samples,0)
  assertthat::assert_that(n>1, msg = "at least 2 samples needed for defining a pairwise swap")
  pos_vec = 1:n
  Z=2:1

  function(...) { # be able to ignore additional params passed to a generic shuffle proposal function
    swap = sample(pos_vec,2)
    list(src=swap, dst=swap[Z])
  }
}

#' Create function to propose n pairwise swaps of samples on each call (n is a constant across iterations)
#'
#' This internal function is wrapped by mk_swapping_function()
#'
#' @param n_samples Total number of samples (i.e. max of permutation index)
#' @param n_swaps Number of swaps to be proposed (valid range is 1..floor(n_samples/2))
#'
#' @return Parameter-less function to return a list with length n vectors 'src' and 'dst', denoting source and destination index for the swap operation on each call
#'
#' @keywords internal
mk_constant_swapping_function = function(n_samples, n_swaps) {
  # Function factory for creator of a 'neighboring' sample arrangement with a defined number of position swaps

  n = round(n_samples,0)
  n_swaps = round(n_swaps,0)
  draws = 2*n_swaps

  if (n < draws) { # limit swaps if user provides a meaningless number
    n_swaps = floor(n/2)
    draws = 2*n_swaps
    message("Re-defined number of swaps to ",n_swaps," in swapping function.")
  }
  assertthat::assert_that(draws>1, msg = "at least 1 swap needed for defining a meaningful swap function")

  pos_vec = 1:n
  Z=c((n_swaps+1):draws, 1:n_swaps) # scrambled return order for destination

  function(...) { # be able to ignore additional params passed to a generic shuffle proposal function
    swap = sample(pos_vec, draws)
    list(src=swap, dst=swap[Z])
  }
}


#' Create function to reshuffle sample indices completely randomly
#'
#' This function was just added to test early on the functionality of optimize_design() to accept a permutation vector rather than a list with src and dst indices.
#'
#' @param n_samples Total number of samples (i.e. max of permutation index)
#'
#' @return Parameter-less function to return a random permutation of the indices in the range 1:n_samples
#'
#' @export
mk_complete_random_shuffling_function = function(n_samples) {
  # Function factory for creator of a complete random reshuffling of samples

  n = round(n_samples,0)
  assertthat::assert_that(n>1, msg = "at least 1 sample needed for defining a meaningful shuffling function")

  pos_vec = 1:n

  function(...) { # be able to ignore additional params passed to a generic shuffle proposal function
    sample(pos_vec)
  }
}


#' Create function to propose swaps of samples on each call, either with a constant number of swaps or following
#' a user defined protocol
#'
#' If length(n_swaps)==1, the returned function may be called an arbitrary number of times.
#' If length(n_swaps)>1 and called without argument, the returned function may be called length(n_swaps) timed before returning NULL, which would be the stopping criterion if all requested swaps have been exhausted. Alternatively, the function may be called with an iteration number as the only argument, giving the user some freedom how to iterate over the sample swapping protocol.
#'
#' @param n_samples Total number of samples (i.e. max of permutation index)
#' @param n_swaps Vector with number of swaps to be proposed in successive calls to the returned function (each value should be in valid range from 1..floor(n_samples/2))
#'
#' @return Function to return a list with length n vectors 'src' and 'dst', denoting source and destination index for the swap operation, or NULL if the user provided a defined protocol for the number of swaps and the last iteration has been reached
#'
#' @export
mk_swapping_function = function(n_samples, n_swaps = 1) {
  # Function factory for creator of a 'neighboring' sample arrangement with a defined number of position swaps

  if (length(n_swaps)==1 && n_swaps==1) { # default to pairwise swapping function in the default case
    return(mk_pairwise_swapping_function(n_samples = n_samples))
  }
  if (length(n_swaps)==1) { # default to function with constant number of swaps in this default case
    return(mk_constant_swapping_function(n_samples = n_samples, n_swaps = n_swaps))
  }

  # User has provided a shuffling protocol!
  n = round(n_samples,0)
  n_swaps = round(n_swaps,0)

  if (any(n_swaps>floor(n/2))) { # limit swaps if user provides a meaningless number
    n_swaps[n_swaps>floor(n/2)] = floor(n/2)
    message("Set upper number of swaps to ",floor(n/2)," in swapping protocol.")
  }
  if (any(n_swaps<1)) {
    n_swaps[n_swaps<1] = 1
    message("Set lower number of swaps to 1 in swapping protocol.")
  }

  pos_vec = 1:n
  iter = 1
  Z = NA
  draws = NA
  S = 0 # remember last number of swaps (for optimizing speed)

  function(iteration=iter) {
    if (iteration>length(n_swaps) || iteration<1) {
      return(NULL)
    }
    ns = n_swaps[iteration]
    if (ns!=S) { # number of swaps changed --> have to set up new params
      draws<<-2*ns
      Z<<-c((ns+1):draws, 1:ns) # scrambled return order for destination
      S<<-ns
    }
    iter<<-iteration+1
    swap = sample(pos_vec, draws)
    list(src=swap, dst=swap[Z])

  }
}

#' Default acceptance function for optimizer (always accept the current score if it is smaller than the best one obtained before)
#'
#' @param current_score Score from the current optimizing iteration (scalar value, double)
#' @param best_score Score from the current optimizing iteration (scalar value, double)
#' @param ... Ignored arguments that may be used by alternative acceptance functions
#'
#' @return Boolean, TRUE if current score should be taken as the new optimal score, FALSE otherwise
#'
#' @keywords internal
accept_best_solution = function(current_score, best_score, ...) { # ignore iteration parameter in case it's passed
  if (current_score<best_score) TRUE else FALSE
}


#' Generic optimizer that can be customized by user provided functions for generating shuffles and progressing towards the minimal score
#'
#' @param batch_container An instance of `BatchContainer`.
#' @param samples A `data.frame` with sample information. Should be `NULL` if the `BatchContainer` already has samples in it.
#' @param n_shuffle Vector of length 1 or larger, defining how many random sample swaps should be performed in each iteration. If length(n_shuffle)==1, this sets no limit to the number of iterations. Otherwise, the optimization stops if the swapping protocol is exhausted.
#' @param shuffle_proposal_func A user defined function to propose the next shuffling of samples. Takes priority over n_shuffle if both are provided. The function is called with one integer parameter for the current iteration number, allowing very flexible shuffling strategies. The returned function must either return a list with fields `src`and `dst` (for pairwise sample swapping) or a numeric vector with a complete re-assigned sample order.
#' @param acceptance_func Alternative function to select a new score as the best one. Defaults to simply taking the overall best score. Max be replaced with an acceptance function generated by mk_simanneal_acceptance_func() or a user provided function.
#' @param max_iter Stop optimization after a maximum number of iterations, independent from other stopping criteria (user defined shuffle proposal or min_score)
#' @param min_score If not NA, optimization is stopped as soon as min_score or lower values are reached
#' @param quiet If TRUE, suppress non-critical warnings or messages.
#'
#' @return A trace object
#'
#' @export
optimize_design <- function(batch_container, samples = NULL, n_shuffle = NULL,
                            shuffle_proposal_func = NULL,
                            acceptance_func = accept_best_solution,
                            max_iter = 1e6, min_score=NA, quiet = FALSE) {

  # New implementation of assign_score_optimize_shuffle()
  # Trace object is not created initially since I have no clue how to test it outside the package context

  start_time <- Sys.time()
  save_random_seed <- .Random.seed

  # Check that samples are available and their number is in valid range.
  # We will use only the unchanged sample list from the batch container (samples_df, NOT samples_dt)
  # and manage the permutation index in this function
  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
                            msg = "batch-container is empty and no samples provided"
    )
  } else {
    assertthat::assert_that(nrow(samples) > 0)
    assign_in_order(batch_container, samples) # don't change initial sample order! side effect: samples are undergoing some checks
  }

  assertthat::assert_that(nrow(batch_container$samples_df)==nrow(batch_container$locations_df),
                          msg = "Situation with non-available container locations is not supported yet!")

  # Check presence of scoring function
  assertthat::assert_that(!is.null(batch_container$scoring_f), msg = "no scoring function set for BatchContainer")

  # Get some stuff just once from the batch container
  loc = batch_container$locations_df
  samp = batch_container$samples_df
  scoring_func = batch_container$scoring_f


  # Create shuffle_proposal_func
  # If passed by the user, this one getting priority over n_shuffle.
  # If nothing is passed, default shuffling function is to swap 2 random elements per iteration, which
  # is implemented by an especially efficient function.
  # Planned: Allow to pass and use functions which instead of returning permutation indices return an entire shuffling
  if (is.null(n_shuffle) && is.null(shuffle_proposal_func)) {
    shuffle_proposal_func = mk_swapping_function(n_samples = nrow(samp), n_swaps = 1)
    shuffle_all = FALSE
  } else if (is.null(shuffle_proposal_func)) {
    shuffle_proposal_func = mk_swapping_function(n_samples = nrow(samp), n_swaps = n_shuffle)
    shuffle_all = FALSE
  } else { # apply but test user provided shuffle function!
    assertthat::assert_that(is.function(shuffle_proposal_func), msg = "shuffle_proposal_func should be a function")
    test_shuffle = shuffle_proposal_func(1) # what would come out in 1st iteration?
    if (is.numeric(test_shuffle)) { # passes back a complete permutation order
      assertthat::assert_that(length(test_shuffle)==nrow(samp), msg = "shuffle proposal function must return a permutation vector of correct length")
      assertthat::assert_that(all(test_shuffle<=nrow(samp)),
                              all(test_shuffle>0),
                              msg = "shuffle proposal function must return valid sample indices")
      shuffle_all = TRUE
    } else { # passes back a list with src and dst for the sample swapping
      assertthat::assert_that(is.list(test_shuffle), msg = "Shuffle proposal function must return either a numeric vector or a list")
      assertthat::assert_that(all(sort(names(test_shuffle))==c("dst","src")), msg = "Shuffle proposal function should return a list with names 'src' and 'dst'")
      assertthat::assert_that(all(unlist(test_shuffle)<=nrow(samp)),
                              all(unlist(test_shuffle)>0),
                              msg = "Shuffle proposal function must return valid sample indices")
      shuffle_all = FALSE
    }
  }

  # Data table for rapid permutation of samples
  samp$.sample_id = 1:nrow(samp) # have to assume this!
  samp_perm = data.table::as.data.table(bind_cols(loc, samp))
  fcols = seq_along(samp) + ncol(loc) # Indices of sample annotation columns to be quickly reshuffled at each iter

  # New index management: we just need one index to reflect sample order
  best_perm = start_perm = samp$.sample_id
  best_score = scoring_func(samp_perm)

  trace <- OptimizationTrace$new(
    max_iter, # Memory usage??? Build trace object in different way
    length(batch_container$aux_scoring_f),
    names(batch_container$aux_scoring_f)
  )

  iteration = 1

  curr_shuffle = shuffle_proposal_func(iteration) # R while loop does not allow variable assignment !? :(

  while (!is.null(curr_shuffle) && (iteration<=max_iter)) { # Some shuffling functions may return NULL to indicate end of permutation protocol

    # avoid sample exchange and scoring functions from container and construct object that is required for scoring directly
    # batch_container$exchange_samples(curr_shuffle$src, curr_shuffle$dst)

    if (shuffle_all) {
      samp_perm[start_perm, (fcols) :=  samp_perm[match(curr_shuffle ,samp_perm$.sample_id), fcols, with = FALSE] ]
    } else {
      samp_perm[curr_shuffle$dst, (fcols) := samp_perm[curr_shuffle$src, fcols, with = FALSE] ]
    }

    new_score = scoring_func(samp_perm)

    if (acceptance_func(new_score, best_score, iteration)) { # only look at main score here, not aux
      best_score = new_score
      best_perm = samp_perm$.sample_id
      if (!quiet) {
        message("Opt. score: ",best_score)
      }
    } else {
      if (!shuffle_all) { # have to swap back!
        samp_perm[curr_shuffle$dst, (fcols) := samp_perm[curr_shuffle$src, fcols, with = FALSE] ]
      }
    }

    trace$set_scores(iteration, best_score)

    # Test stopping criteria
    if (!is.na(min_score) && best_score<=min_score) {
      if (!quiet) { message("Reached min_score in ",iteration," iterations.") }
      break
    }

    iteration = iteration+1
    curr_shuffle=shuffle_proposal_func(iteration)

  }

  batch_container$assignment_vec = best_perm

  trace$seed <- save_random_seed
  trace$elapsed <- Sys.time() - start_time
  trace
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
