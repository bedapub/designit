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
  if (!exists(".Random.seed")) stats::runif(1)
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

  assertthat::assert_that(rlang::is_integerish(n_samples, n = 1, finite = TRUE),
                          msg = "n_samples should be a single iteger")
  assertthat::assert_that(n_samples > 1, msg = "at least 2 samples needed for defining a pairwise swap")
  pos_vec <- 1:n_samples
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

  assertthat::assert_that(rlang::is_integerish(n_samples, n = 1, finite = TRUE),
                          msg = "n_samples should be a single iteger")
  assertthat::assert_that(rlang::is_integerish(n_swaps, finite = TRUE),
                          msg = "n_swaps should be an iteger vector")
  n <- n_samples
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
    # ensures that there are
    # a) no samples are left in place
    # b) no complex shuffles, like 1->2, 2->3, 3->1
    list(src = swap, dst = swap[Z])
  }
}


#' Create function to reshuffle sample indices completely randomly
#'
#' This function was just added to test early on the functionality of optimize_design() to accept a permutation vector rather than a list with src and dst indices.
#'
#' @param batch_container A batch container with samples assigned that should be permuted randomly
#'
#' @return Parameter-less function to return a random permutation of the sample locations in the container
#'
#' @export
mk_complete_random_shuffling_function <- function(batch_container) {
  # Function factory for creator of a complete random reshuffling of samples

  assertthat::assert_that(batch_container$has_samples, msg = "Batch container has to have samples assigned")

  pos_vec <- batch_container$assignment

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
#' @return Function to return a list with length n vectors `src` and `dst`, denoting source and destination index for the swap operation, or NULL if the user provided a defined protocol for the number of swaps and the last iteration has been reached
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
  assertthat::assert_that(rlang::is_integerish(n_samples, n = 1, finite = TRUE),
                          msg = "n_samples should be a single iteger")
  assertthat::assert_that(rlang::is_integerish(n_swaps, finite = TRUE),
                          msg = "n_swaps should be an iteger vector")
  n <- n_samples

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


#' Created a shuffling function that permutes samples within certain subgroups of the container locations
#'
#' If length(n_swaps)==1, the returned function may be called an arbitrary number of times.
#' If length(n_swaps)>1 the returned function may be called length(n_swaps) timed before returning NULL, which would be the stopping criterion if all requested swaps have been exhausted.
#'
#' @param batch_container Batch container with locations that can be used to define subgroups
#' @param subgroup_vars Column names of the variables that together define the relevant subgroups
#' @param restrain_on_subgroup_levels Permutations can be forced to take place only within a level of the factor of the subgrouping variable. In this case, the user must pass only one subgrouping variable and a number of levels that together define the permuted subgroup.
#' @param n_swaps Vector with number of swaps to be proposed in successive calls to the returned function (each value should be in valid range from 1..floor(n_locations/2))
#'
#' @return Function to return a list with length n vectors 'src' and 'dst', denoting source and destination index for the swap operation, or NULL if the user provided a defined protocol for the number of swaps and the last iteration has been reached
#' @export
#'
mk_subgroup_shuffling_function = function(batch_container, subgroup_vars,
                                          restrain_on_subgroup_levels = c(),
                                          n_swaps=1) {

  MAX_PERMUTATIONS = 1e6 # limit memory use of this function

  bc_loc = batch_container$get_locations()
  assertthat::assert_that(nrow(bc_loc)>9, msg="Subgroup shuffling is pointless for small containers (n<10)")
  assertthat::assert_that(all(subgroup_vars %in% colnames(bc_loc)), msg="All subgroup defining variables have to be part of the container locations")

  assertthat::assert_that(nrow(dplyr::filter(dplyr::select(bc_loc, dplyr::all_of(subgroup_vars)),
                                             dplyr::if_any(dplyr::everything(), ~ !is.na(.))))==nrow(bc_loc),
                          msg="Selected subgrouping variables should not contain any NA values")

  assertthat::assert_that( !(!is.null(restrain_on_subgroup_levels) && length(restrain_on_subgroup_levels)>0 && length(subgroup_vars)!=1),
                           msg="Exactly one subgrouping variable must be specified if specific subgrouping levels are passed")
  assertthat::assert_that( is.null(restrain_on_subgroup_levels) || length(restrain_on_subgroup_levels)==0 ||
                           all(restrain_on_subgroup_levels %in% bc_loc[[subgroup_vars]]) ,
                           msg="All selected subgroup levels have to be present in the subgrouping variable")

  if (!is.null(restrain_on_subgroup_levels) && length(restrain_on_subgroup_levels)>0) { # we focus on selected subgroups only
    valid_indices = which( bc_loc[[subgroup_vars]] %in% restrain_on_subgroup_levels)
    subgroup_sizes = length(valid_indices)
    n_permut = subgroup_sizes*(subgroup_sizes-1)/2
    assertthat::assert_that(n_permut<=MAX_PERMUTATIONS,
                            msg=stringr::str_c("Subgroup shuffling would lead to more than ", MAX_PERMUTATIONS,
                                               " possible permutations. Consider a different solution."))
    valid_permutations = tidyr::crossing( src=valid_indices, dst=valid_indices) %>% dplyr::filter(src<dst)
  } else { # we swap samples across subgroups
    bc_loc = dplyr::group_by(bc_loc, dplyr::across(dplyr::all_of(subgroup_vars)))
    grp_ind = dplyr::group_indices(bc_loc)
    subgroup_sizes = dplyr::group_size(bc_loc)
    n_permut = sum(subgroup_sizes*(subgroup_sizes-1)/2)
    assertthat::assert_that(n_permut<=MAX_PERMUTATIONS,
                            msg=stringr::str_c("Subgroup shuffling would lead to more than ", MAX_PERMUTATIONS,
                                               " possible permutations. Consider a different solution."))
    assertthat::assert_that(length(subgroup_sizes)>1, msg="Subgroup shuffling is pointless if there's only one subgroup involved")
    valid_permutations = purrr::map(seq_along(subgroup_sizes), ~ which(grp_ind==.x)) %>%
      purrr::map( ~tidyr::crossing( src=.x, dst=.x) %>% dplyr::filter(src<dst)) %>%
      dplyr::bind_rows()
  }

  assertthat::assert_that(all(subgroup_sizes>1), msg="Subgroup shuffling requires all subgroups to have a minimum size of 2")

  assertthat::assert_that(n_permut==nrow(valid_permutations), msg="Permutation calculations screwed up. Check the code.")

  valid_indices = 1:n_permut

  # Check user provided shuffling protocol
  n_swaps <- round(n_swaps, 0)

  if (any(n_swaps > n_permut)) { # limit swaps if user provides a meaningless number
    n_swaps[n_swaps > n_permut] <- n_permut
    message("Set upper number of swaps to ", n_permut, " in swapping protocol.")
  }
  if (any(n_swaps < 1)) {
    n_swaps[n_swaps < 1] <- 1
    message("Set lower number of swaps to 1 in swapping protocol.")
  }

  # Helper function to pick n INDEPENDENT permutations, i.e. permutations that don't lead to sample loss
  pick_indep_perm = function(n) {
    # Start with an index of all possible permutations
    poss_perm = valid_permutations
    source = desti = integer(n)
    for (i in 1:n) {
      p = floor(stats::runif(1,1,nrow(poss_perm)+1))
      source[i]=poss_perm[["src"]][p]
      desti[i]=poss_perm[["dst"]][p]
      poss_perm = dplyr::filter(poss_perm, src!=source[i], src!=desti[i], dst!=source[i], dst!=desti[i] )
      if (nrow(poss_perm)==0) {
        # Stop if we don't have any independent exchanges left
        break
      }
    }
    list(src=c(source[1:i], desti[1:i]), dst=c(desti[1:i], source[1:i]))
  }


  iter <- 1

  if (length(n_swaps)==1) {
    return(
      function(...) { # ignore possible other params passed to a generic shuffle function

        if (n_swaps==1) {
          swap = sample(valid_indices, 1)
          return(list(src = c(valid_permutations[["src"]][swap], valid_permutations[["dst"]][swap]),
                      dst = c(valid_permutations[["dst"]][swap], valid_permutations[["src"]][swap])))
        } else {
          return(pick_indep_perm(n_swaps))
        }
      }
    )
  }

  function(iteration = iter, ...) { # ignore possible other params passed to a generic shuffle function
    if (iteration > length(n_swaps) || iteration < 1) {
      return(NULL)
    }
    iter <<- iter+1

    if (n_swaps[iteration]==1) {
      swap = sample(valid_indices, 1)
      return(list(src = c(valid_permutations[["src"]][swap], valid_permutations[["dst"]][swap]),
                  dst = c(valid_permutations[["dst"]][swap], valid_permutations[["src"]][swap])))
    } else {
      return(pick_indep_perm(n_swaps[iteration]))
    }
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
#' @param samples A `data.frame` with sample information.
#' Should be `NULL` if the `BatchContainer` already has samples in it.
#' @param n_shuffle Vector of length 1 or larger, defining how many random sample
#' swaps should be performed in each iteration. If length(n_shuffle)==1,
#' this sets no limit to the number of iterations. Otherwise, the optimization
#' stops if the swapping protocol is exhausted.
#' @param shuffle_proposal_func A user defined function to propose the next shuffling of samples.
#' Takes priority over n_shuffle if both are provided. The function is called with
#' one integer parameter for the current iteration number, allowing very flexible shuffling strategies.
#' The returned function must either return a list with fields `src`and `dst` (for pairwise sample swapping)
#' or a numeric vector with a complete re-assigned sample order.
#' @param acceptance_func Alternative function to select a new score as the best one.
#' Defaults to simply taking the overall best score. Max be replaced with an
#' acceptance function generated by mk_simanneal_acceptance_func() or a user provided function.
#' @param aggregate_scores_func A function to aggregate the scores.
#' By default one is used that just uses the first score.
#' @param max_iter Stop optimization after a maximum number of iterations,
#' independent from other stopping criteria (user defined shuffle proposal or min_score)
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
  start_time <- Sys.time()

  # based on https://stat.ethz.ch/pipermail/r-help/2007-September/141717.html
  if (!exists(".Random.seed")) stats::runif(1)
  save_random_seed <- .Random.seed

  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
                            msg = "batch-container is empty and no samples provided"
    )
  } else {
    assertthat::assert_that(nrow(samples) > 0)
    assign_in_order(batch_container, samples)
  }


  # Check presence of scoring function and that it's a list of functions
  assertthat::assert_that(!is.null(batch_container$scoring_f), msg = "no scoring function set for BatchContainer")
  assertthat::assert_that(is.list(batch_container$scoring_f), msg = "scoring function is expected to be a list")
  assertthat::assert_that(all(purrr::map_lgl(batch_container$scoring_f, is.function)), msg = "All scoring functions have to be function definitions")


  # Get assigned samples and locations from the batch container
  samp <- batch_container$get_samples(include_id = TRUE, assignment = TRUE, remove_empty_locations = FALSE)
  n_samples <- length(stats::na.exclude(samp$.sample_id))
  n_locations <- nrow(samp)

  assertthat::assert_that(".sample_id" %in% colnames(samp),
                          all(sort(samp$.sample_id, na.last = NA) == 1:n_samples),
                          msg = stringr::str_c(".sample_id from batch container must exist and numerate samples from 1 to ", n_samples)
  )

  # Create shuffle_proposal_func
  # If passed by the user, this one getting priority over n_shuffle.
  # If nothing is passed, default shuffling function is to swap 2 random elements per iteration, which
  # is implemented by an especially efficient function.
  if (is.null(n_shuffle) && is.null(shuffle_proposal_func)) {
    shuffle_proposal_func <- mk_swapping_function(n_samples = nrow(samp), n_swaps = 1)
  } else if (is.null(shuffle_proposal_func)) {
    shuffle_proposal_func <- mk_swapping_function(n_samples = nrow(samp), n_swaps = n_shuffle)
    if (length(n_shuffle) > 1) {
      # Restrict number if iters, so that also trace object will be appropriately sized
      max_iter <- min(max_iter, length(n_shuffle), na.rm = T)
    }
  }

  assertthat::assert_that(is.function(shuffle_proposal_func), msg = "shuffle_proposal_func should be a function")


  using_attributes <- FALSE # keeps track if attributes had been used in 1st iteration, since they must be provided consistently

  extract_shuffle_params = function(shuffle) {
    # Extracts relevant parameters from shuffle function output and monitors correctness/consistency
    # Tried to avoid redundant checks that are performed on batch container level

    # Any shuffling function should return one of the following
    # 1. atomic index vector for a direct location assignment
    # 2. a list with src and dst vectors
    # 3. a list with locations vector (for location assignment) and optional sample_attr data frame/tibble

    if (is.null(shuffle)) { # marks end of iteration schedule
      return(NULL)
    }

    if (rlang::is_atomic(shuffle)) {
      loc = shuffle
      src = dst = attrib = NULL
      assertthat::assert_that(!using_attributes,
                              msg = "sample attributes must be consistently supplied by shuffle function once started")
    } else {
      assertthat::assert_that(is.list(shuffle), msg = "shuffle proposal function must return either a numeric vector or a list")
      if (!is.null(shuffle[["src"]]) && !is.null(shuffle[["dst"]])) {
        loc = NULL
        src = shuffle[["src"]]
        dst = shuffle[["dst"]]
      } else {
        assertthat::assert_that(!is.null(shuffle[["location_assignment"]]), msg="shuffle function must return either a src/dst pair or a location vector")
        loc = shuffle[["location_assignment"]]
        src = dst = NULL
      }
      if (is.null(shuffle[["samples_attr"]])) {
        assertthat::assert_that(!using_attributes,
                                msg = "sample attributes must be consistently supplied by shuffle function once started")
        attrib = NULL
      } else {
        attrib = shuffle[["samples_attr"]]
        using_attributes <<- TRUE
      }
    }

    list( src=src, dst=dst, location_assignment=loc, samples_attr=attrib)
  }

  attrib_msg_made <- FALSE

  update_batchcontainer = function(shuffle_params) {

    batch_container$move_samples(src = shuffle_params$src, dst = shuffle_params$dst , location_assignment = shuffle_params$location_assignment )

    # Add sample attributes to container if necessary
    if (!is.null(shuffle_params[["samples_attr"]])) {
      batch_container$samples_attr = shuffle_params[["samples_attr"]]
      if (!quiet && !attrib_msg_made) {
        message( "Adding ", ncol(shuffle_params[["samples_attr"]]), " attributes to samples.")
        attrib_msg_made <<- TRUE
      }
    }
  }


  iteration <- 1
  shuffle_params <- shuffle_proposal_func(iteration) %>% extract_shuffle_params()


  # If sample attributes are provided, frontload first bc update since additional variables may be actually used in the scoring function(s)!
  # Would be nice in principle to check whether any of these variable is ACTUALLY used in a scoring function
  if (using_attributes) {
    if (!quiet) {
      message("Permutation function uses sample attributes. Frontloading sample permutation before scoring.")
    }
    update_batchcontainer(shuffle_params)
  }

  # Remember initial sample order as best permutation so far and calculate multi-variate score
  best_perm <- batch_container$assignment
  best_score <- batch_container$score()
  best_agg <- aggregate_scores_func(best_score)
  score_dim <- length(best_score)

  trace <- OptimizationTrace$new(
    max_iter + 1, # + 1 to accommodate initial score
    length(batch_container$scoring_f),
    names(batch_container$scoring_f)
  )

  trace$set_scores(1, best_score)

  if (!quiet) {
    message("Initial aggregated score: ", best_agg, " (", score_dim, "-dim)",
            ifelse(score_dim < 2, "", stringr::str_c(" [c(", stringr::str_c(round(best_score, 3), collapse = ", "), ")]")))
  }


  while (!is.null(shuffle_params) && (iteration <= max_iter)) { # NULL may indicate end of permutation protocol

    update_batchcontainer(shuffle_params)

    new_score <- batch_container$score()
    assertthat::assert_that(!any(is.na(new_score)), msg=stringr::str_c("NA apprearing during scoring in iteration ", iteration))

    if (acceptance_func(aggregate_scores_func(new_score), best_agg, iteration)) {
      best_score <- new_score
      best_agg <- aggregate_scores_func(best_score)
      best_perm <- batch_container$assignment
      if (!quiet) {
        message(
          "Achieved score: ", best_agg,
          ifelse(score_dim < 2, "", stringr::str_c(" [c(", stringr::str_c(round(best_score, 3), collapse = ", "), ")]")),
          " in iter ", iteration
        )

      }
    } else {
      if (is.null(shuffle_params[["location_assignment"]])) { # we used the permutation method and thus have to swap samples back!
        batch_container$move_samples(src = shuffle_params$src, dst = shuffle_params$dst)
      }
    }

    iteration <- iteration + 1
    trace$set_scores(iteration, best_score)

    # Test stopping criteria
    if (!is.na(min_score) && best_agg <= min_score) {
      if (!quiet) {
        message("Reached min_score in ", iteration-1, " iterations.")
      }
      break
    }

    shuffle_params <- shuffle_proposal_func(iteration) %>% extract_shuffle_params()

  }

  # In the end, always make sure that final state of bc is the one with the best score
  batch_container$move_samples(location_assignment = best_perm)

  trace$shrink(iteration)
  trace$seed <-  save_random_seed
  trace$elapsed <- Sys.time() - start_time
  trace
}
