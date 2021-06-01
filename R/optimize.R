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
#' A function used to propose a shuffle in every step.
#' If non-`NULL` a function receives two arguments on every iteration:
#' `bc$get_samples(include_id = TRUE, as_tibble = FALSE)` and the iteration number.
#' This function should return a list with attributes `src` and `dst`
#' (see [`BatchContainer$move_samples()`][BatchContainer]).
#' There is no support for `location_assignment` currently.
#' @param iterations Number of iterations. If not provided set to 1000.
#' @param aggregate_scores_func A function to aggregate the scores.
#' By default one is used that just uses the first score.
#' @return An [OptimizationTrace] object.
#' @export
assign_score_optimize_shuffle <- function(batch_container, samples = NULL, n_shuffle = NULL, shuffle_proposal = NULL, iterations = NULL, aggregate_scores_func = first_score_only) {
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
    length(batch_container$scoring_f),
    names(batch_container$scoring_f)
  )
  current_score <- batch_container$score()
  trace$set_scores(1, current_score)

  for (i in seq_len(iterations)) {
    perm <- seq_len(n_avail)
    if (is.function(shuffle_proposal)) {
      for (j in seq_len(n_shuffle[i])) {
        sh <- shuffle_proposal(batch_container$get_samples(include_id = TRUE, as_tibble = FALSE), i)
        assertthat::assert_that(is.list(sh), msg = "Shuffle proposal function should return a list")
        src <- sh$src
        dst <- sh$dst
        if (is.null(src)) {
          break
        }
        perm[dst] <- perm[src]
        batch_container$move_samples(src, dst)
      }
    } else {
      non_empty_loc <- which(!is.na(batch_container$assignment))
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
      batch_container$move_samples(src, dst)
    }

    non_trivial <- which(perm != seq_along(perm))
    if (length(non_trivial) == 0) {
      # the shuffling is a trivial permutation, go to the next iteration
      trace$set_scores(i + 1, current_score)
      next
    }

    new_score <- batch_container$score()
    if (aggregate_scores_func(new_score) >= aggregate_scores_func(current_score)) {
      batch_container$move_samples(non_trivial, perm[non_trivial])
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
mk_pairwise_swapping_function <- function(n_samples) {
  # Function factory for creator of a 'neighboring' sample arrangement with just one pairwise swap

  n <- round(n_samples, 0)
  assertthat::assert_that(n > 1, msg = "at least 2 samples needed for defining a pairwise swap")
  pos_vec <- 1:n
  Z <- 2:1

  function(...) { # be able to ignore additional params passed to a generic shuffle proposal function
    swap <- sample(pos_vec, 2)
    list(src = swap, dst = swap[Z])
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
mk_constant_swapping_function <- function(n_samples, n_swaps) {
  # Function factory for creator of a 'neighboring' sample arrangement with a defined number of position swaps

  n <- round(n_samples, 0)
  n_swaps <- round(n_swaps, 0)
  draws <- 2 * n_swaps

  if (n < draws) { # limit swaps if user provides a meaningless number
    n_swaps <- floor(n / 2)
    draws <- 2 * n_swaps
    message("Re-defined number of swaps to ", n_swaps, " in swapping function.")
  }
  assertthat::assert_that(draws > 1, msg = "at least 1 swap needed for defining a meaningful swap function")

  pos_vec <- 1:n
  Z <- c((n_swaps + 1):draws, 1:n_swaps) # scrambled return order for destination

  function(...) { # be able to ignore additional params passed to a generic shuffle proposal function
    swap <- sample(pos_vec, draws)
    list(src = swap, dst = swap[Z])
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
mk_complete_random_shuffling_function <- function(n_samples) {
  # Function factory for creator of a complete random reshuffling of samples

  n <- round(n_samples, 0)
  assertthat::assert_that(n > 1, msg = "at least 1 sample needed for defining a meaningful shuffling function")

  pos_vec <- 1:n

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
mk_swapping_function <- function(n_samples, n_swaps = 1) {
  # Function factory for creator of a 'neighboring' sample arrangement with a defined number of position swaps

  if (length(n_swaps) == 1 && n_swaps == 1) { # default to pairwise swapping function in the default case
    return(mk_pairwise_swapping_function(n_samples = n_samples))
  }
  if (length(n_swaps) == 1) { # default to function with constant number of swaps in this default case
    return(mk_constant_swapping_function(n_samples = n_samples, n_swaps = n_swaps))
  }

  # User has provided a shuffling protocol!
  n <- round(n_samples, 0)
  n_swaps <- round(n_swaps, 0)

  if (any(n_swaps > floor(n / 2))) { # limit swaps if user provides a meaningless number
    n_swaps[n_swaps > floor(n / 2)] <- floor(n / 2)
    message("Set upper number of swaps to ", floor(n / 2), " in swapping protocol.")
  }
  if (any(n_swaps < 1)) {
    n_swaps[n_swaps < 1] <- 1
    message("Set lower number of swaps to 1 in swapping protocol.")
  }

  pos_vec <- 1:n
  iter <- 1
  Z <- NA
  draws <- NA
  S <- 0 # remember last number of swaps (for optimizing speed)

  function(iteration = iter, ...) { # ignore possible other params passed to a generic shuffle function
    if (iteration > length(n_swaps) || iteration < 1) {
      return(NULL)
    }
    ns <- n_swaps[iteration]
    if (ns != S) { # number of swaps changed --> have to set up new params
      draws <<- 2 * ns
      Z <<- c((ns + 1):draws, 1:ns) # scrambled return order for destination
      S <<- ns
    }
    iter <<- iteration + 1
    swap <- sample(pos_vec, draws)
    list(src = swap, dst = swap[Z])
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
accept_best_solution <- function(current_score, best_score, ...) { # ignore iteration parameter in case it's passed
  current_score < best_score
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
                            aggregate_scores_func = first_score_only,
                            max_iter = 1e4, min_score = NA, quiet = FALSE) {

  # This is an alternative implementation to assign_score_optimize_shuffle()

  start_time <- Sys.time()
  save_random_seed <- .Random.seed # Symbol not defined unless user has called set.seed() !! Should change.


  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
      msg = "batch-container is empty and no samples provided"
    )
  } else {
    assertthat::assert_that(nrow(samples) > 0)
    assign_in_order(batch_container, samples) # don't change initial sample order! side effect: samples are undergoing some checks
  }


  # Check presence of scoring function; convert to a list if needed since we evaluate with map_dbl
  assertthat::assert_that(!is.null(batch_container$scoring_f), msg = "no scoring function set for BatchContainer")
  scoring_func <- batch_container$scoring_f
  if (!is.list(scoring_func)) {
    scoring_func <- list(scoring_func)
  }
  assertthat::assert_that(all(purrr::map_lgl(scoring_func, is.function)), msg = "All scoring functions have to be function definitions")

  # Get some stuff just once from the batch container
  loc <- batch_container$locations_df
  samp_perm <- data.table::copy(batch_container$samples_dt) # includes empty positions!
  assertthat::assert_that(".sample_id" %in% colnames(samp_perm),
    all(sort(samp_perm$.sample_id, na.last = NA) == 1:nrow(batch_container$samples_df)),
    msg = stringr::str_glue(
      ".sample_id from batch container must exist and numerate samples from 1 to ",
      nrow(batch_container$samples_df)
    )
  )

  append_to_data <- FALSE
  shuffle_all <- FALSE

  # Create shuffle_proposal_func
  # If passed by the user, this one getting priority over n_shuffle.
  # If nothing is passed, default shuffling function is to swap 2 random elements per iteration, which
  # is implemented by an especially efficient function.

  if (is.null(n_shuffle) && is.null(shuffle_proposal_func)) {
    shuffle_proposal_func <- mk_swapping_function(n_samples = nrow(samp_perm), n_swaps = 1)
  } else if (is.null(shuffle_proposal_func)) {
    shuffle_proposal_func <- mk_swapping_function(n_samples = nrow(samp_perm), n_swaps = n_shuffle)
    if (length(n_shuffle) > 1) {
      # Restrict number if iters, so that also trace object is appropriately sized
      max_iter <- min(max_iter, length(n_shuffle), na.rm = T)
    }
  } else { # apply but test user provided shuffle function!
    assertthat::assert_that(is.function(shuffle_proposal_func), msg = "shuffle_proposal_func should be a function")
    test_shuffle <- shuffle_proposal_func(iteration = 1) # what would come out in 1st iteration?
    if (is.numeric(test_shuffle)) { # passes back a complete permutation order
      assertthat::assert_that(length(test_shuffle) == nrow(samp_perm),
        msg = "shuffle proposal function must return a permutation vector of correct length"
      )
      assertthat::assert_that(all(test_shuffle <= nrow(samp_perm)),
        all(test_shuffle > 0),
        msg = "shuffle proposal function must return valid sample indices"
      )
      shuffle_all <- TRUE
    } else { # passes back a list with src and dst for the sample swapping
      assertthat::assert_that(is.list(test_shuffle), msg = "Shuffle proposal function must return either a numeric vector or a list")
      assertthat::assert_that(all(sort(names(test_shuffle)) == c("dst", "src")), msg = "Shuffle proposal function should return a list with names 'src' and 'dst'")
      assertthat::assert_that(all(unlist(test_shuffle) <= nrow(samp_perm)),
        all(unlist(test_shuffle) > 0),
        msg = "Shuffle proposal function must return valid sample indices"
      )
    }

    test_shuffle <- shuffle_proposal_func(onlyShuffleIndex = F, bufferedShuffle = T) # can we get additional columns for the data frame?
    if (is.list(test_shuffle) && "DATA_APPEND" %in% names(test_shuffle)) {
      append_to_data <- TRUE
    }
  }

  # Data table for rapid permutation of samples
  fcols <- which(!colnames(samp_perm) %in% colnames(loc)) # Indices of sample annotation columns to be quickly reshuffled at each iter

  # Remember initial sample order as best permutation so far and calculate multi-variate score
  best_perm <- samp_perm # just makes a reference at this point
  best_score <- purrr::map_dbl(scoring_func, ~ .x(best_perm))
  best_agg <- aggregate_scores_func(best_score)
  score_dim <- length(best_score)

  trace <- OptimizationTrace$new(
    max_iter, # Memory usage???
    length(batch_container$aux_scoring_f),
    names(batch_container$aux_scoring_f)
  )

  iteration <- 1

  curr_shuffle <- shuffle_proposal_func(iteration) # R while loop does not allow variable assignment !? :(
  if (append_to_data) { # Note that these columns are NOT reshuffled, but can be joined to the initial data frame!
    append_cols <- shuffle_proposal_func(onlyShuffleIndex = F, bufferedShuffle = T)[["DATA_APPEND"]]
  }

  if (!quiet) {
    message(
      "Initial aggregated score: ", best_agg, " (", score_dim, "-dim), shuffling ", ifelse(shuffle_all, "ALL", "SUBSETS"),
      ifelse(append_to_data, ", appending extra sample columns", "")
    )
  }

  while (!is.null(curr_shuffle) && (iteration <= max_iter)) { # Some shuffling functions may return NULL to indicate end of permutation protocol

    # avoid sample exchange and scoring functions from container and construct object that is required for scoring directly

    if (shuffle_all) {
      samp_perm[1:nrow(samp_perm), (fcols) := samp_perm[match(curr_shuffle, samp_perm$.sample_id), fcols, with = FALSE]]
    } else {
      samp_perm[curr_shuffle$dst, (fcols) := samp_perm[curr_shuffle$src, fcols, with = FALSE]]
    }

    new_score <- purrr::map_dbl(scoring_func, ~ .x(samp_perm))

    if (acceptance_func(aggregate_scores_func(new_score), best_agg, iteration)) {
      best_score <- new_score
      best_agg <- aggregate_scores_func(best_score)
      best_perm <- data.table::copy(samp_perm) # make a physical copy in memory!
      if (!quiet) {
        message(
          "Achieved score: ", best_agg,
          ifelse(score_dim < 2, "", stringr::str_c(" [c(", stringr::str_c(round(best_score, 3), collapse = ", "), ")]")),
          " in iter ", iteration
        )
      }
      if (append_to_data) {
        append_cols <- shuffle_proposal_func(onlyShuffleIndex = F, bufferedShuffle = T)[["DATA_APPEND"]]
      }
    } else {
      if (!shuffle_all) { # have to swap back!
        samp_perm[curr_shuffle$dst, (fcols) := samp_perm[curr_shuffle$src, fcols, with = FALSE]]
      }
    }

    trace$set_scores(iteration, best_agg) # propose to store vector of scores, too

    # Test stopping criteria
    if (!is.na(min_score) && best_agg <= min_score) {
      if (!quiet) {
        message("Reached min_score in ", iteration, " iterations.")
      }
      break
    }

    iteration <- iteration + 1
    curr_shuffle <- shuffle_proposal_func(iteration)
  }

  if (append_to_data) { # In case we append new sample columns, re-define samples_df in the batch container
    best_perm <- bind_cols(
      dplyr::select(best_perm, -any_of(c(batch_container$dimension_names, colnames(append_cols), ".sample_id"))),
      append_cols
    ) %>% # strip off container names and potentially already existing append columns
      as.data.frame()
    if (!quiet) {
      message(ncol(append_cols), " columns added to batch container data frame.")
    }
    batch_container$samples_df <- best_perm
  } else { # If nothing has been added to the sample list (the regular case), just update the container positions
    batch_container$samples_dt <- best_perm # should be safe just to pass the reference, or?
    batch_container$assignment_vec <- best_perm$.sample_id
  }

  trace$seed <- save_random_seed
  trace$elapsed <- Sys.time() - start_time
  trace
}
