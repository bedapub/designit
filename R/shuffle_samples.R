#' Proposes pairwise swap of samples on each call.
#'
#' This function will ensure that one of the locations is always non-empty. It should not
#' return trivial permutations (e.g., `src=c(1,2)` and `dst=c(1,2)`).
#'
#' @param batch_container The batch-container.
#' @param iteration The current iteration number.
#'
#' @return Function accepting batch container & iteration number. It returns a list with length 1 vectors 'src' and 'dst', denoting source and destination index for the swap operation
#'
#' @keywords internal
pairwise_swapping <- function(batch_container, iteration) {
  # We assume that optimization time is usually dominated by the scoring function,
  # so we do not try to cache values in this function.
  non_empty_locations <- which(!is.na(batch_container$assignment))
  first_element <- sample(non_empty_locations, 1)
  non_first_location <- seq_len(batch_container$n_locations)[-first_element]
  second_element <- sample(non_first_location, 1)
  return(list(src = c(first_element, second_element), dst = c(second_element, first_element)))
}

#' Create function to propose n pairwise swaps of samples on each call (n is a constant across iterations)
#'
#' This internal function is wrapped by mk_swapping_function()
#'
#' @param n_swaps Number of swaps to be proposed (valid range is 1..floor(n_samples/2))
#' @param quiet Do not warn if number of swaps is too big.
#'
#' @return Function accepting batch container & iteration number.
#' Return a list with length n vectors 'src' and 'dst', denoting source and destination index for
#' the swap operation on each call
#'
#' @keywords internal
mk_constant_swapping_function <- function(n_swaps, quiet = FALSE) {
  # Function factory for creator of a 'neighboring' sample arrangement with a defined number of position swaps
  force(n_swaps)
  force(quiet)
  draws <- NULL

  function(batch_container, iteration) {
    if (is.null(draws)) {
      # first time we make sure that n_swaps makes sense
      redefined <- FALSE
      if (n_swaps > nrow(batch_container$samples)) {
        n_swaps <<- nrow(batch_container$samples)
        redefined <- TRUE
      }
      draws <<- 2 * n_swaps
      if (draws > batch_container$n_locations) {
        n_swaps <<- floor(batch_container$n_locations / 2)
        draws <<- 2 * n_swaps
        redefined <- TRUE
      }
      assertthat::assert_that(draws > 1, msg = "at least 1 swap needed for defining a meaningful swap function")
      if (redefined && !quiet) {
        message("Re-defined number of swaps to ", n_swaps, " in swapping function.")
      }
    }
    non_empty_locations <- which(!is.na(batch_container$assignment))
    first_elements <- sample(non_empty_locations, n_swaps)
    non_first_location <- seq_len(batch_container$n_locations)[-first_elements]
    second_elements <- sample(non_first_location, n_swaps)

    # ensures that there are
    # a) no samples are left in place
    # b) no complex shuffles, like 1->2, 2->3, 3->1
    list(src = c(first_elements, second_elements), dst = c(second_elements, first_elements))
  }
}


#' Reshuffle sample indices completely randomly
#'
#' This function was just added to test early on the functionality of optimize_design() to accept a
#' permutation vector rather than a list with src and dst indices.
#'
#' @param batch_container The batch-container.
#' @param ... Other params that are passed to a generic shuffling function (like the iteration number).
#'
#' @return A random permutation of the sample assignment in the container.
#'
#' @export
#'
#' @examples
#' data("invivo_study_samples")
#' bc <- BatchContainer$new(
#'   dimensions = c("plate" = 2, "column" = 5, "row" = 6)
#' )
#' scoring_f <- osat_score_generator("plate", "Sex")
#' bc <- optimize_design(
#'   bc, scoring = scoring_f, invivo_study_samples,
#'   max_iter = 100,
#'   shuffle_proposal_func = complete_random_shuffling
#' )
complete_random_shuffling <- function(batch_container, ...) {
  sample(batch_container$assignment)
}


#' Create function to propose swaps of samples on each call, either with a constant number of swaps or following
#' a user defined protocol
#'
#' If `length(n_swaps)==1`, the returned function may be called an arbitrary number of times.
#' If `length(n_swaps)>1` and called without argument, the returned function may be called length(n_swaps) timed before returning NULL, which would be the stopping criterion if all requested swaps have been exhausted. Alternatively, the function may be called with an iteration number as the only argument, giving the user some freedom how to iterate over the sample swapping protocol.
#'
#' @param n_swaps Vector with number of swaps to be proposed in successive calls to the returned function (each value should be in valid range from 1..`floor(n_samples/2)`)
#'
#' @return Function to return a list with length n vectors `src` and `dst`, denoting source and destination index for the swap operation, or NULL if the user provided a defined protocol for the number of swaps and the last iteration has been reached.
#'
#' @export
#'
#' @examples
#' data("invivo_study_samples")
#' bc <- BatchContainer$new(
#'   dimensions = c("plate" = 2, "column" = 5, "row" = 6)
#' )
#' scoring_f <- osat_score_generator("plate", "Sex")
#' optimize_design(
#'   bc, scoring = scoring_f, invivo_study_samples,
#'   max_iter = 100,
#'   shuffle_proposal_func = mk_swapping_function(1)
#' )
mk_swapping_function <- function(n_swaps = 1) {
  # Function factory for creator of a 'neighboring' sample arrangement with a defined number of position swaps

  if (length(n_swaps) == 1 && n_swaps == 1) { # default to pairwise swapping function in the default case
    return(pairwise_swapping)
  }
  if (length(n_swaps) == 1) { # default to function with constant number of swaps in this default case
    return(mk_constant_swapping_function(n_swaps = n_swaps))
  }

  # User has provided a shuffling protocol!
  assertthat::assert_that(rlang::is_integerish(n_swaps, finite = TRUE),
    msg = "n_swaps should be an iteger vector"
  )

  swapping_functions <- NULL
  if (length(unique(n_swaps)) < 1000) {
    # When number of unique values is small, pre-generate a swapping function for each n_swaps.
    swapping_functions <- n_swaps |>
      unique() |>
      purrr::set_names() |>
      purrr::map(mk_constant_swapping_function)
  }


  function(batch_container, iteration) {
    assertthat::assert_that(iteration <= length(n_swaps))
    if (!is.null(swapping_functions)) {
      f <- swapping_functions[[as.character(n_swaps[iteration])]]
    } else {
      # call the function in quiet mode to avoid too much output
      f <- mk_constant_swapping_function(n_swaps[iteration], quiet = TRUE)
    }
    assertthat::assert_that(!is.null(f))
    return(f(batch_container, iteration))
  }
}


#' Created a shuffling function that permutes samples within certain subgroups of the container locations
#'
#' If length(n_swaps)==1, the returned function may be called an arbitrary number of times.
#' If length(n_swaps)>1 the returned function may be called length(n_swaps) timed before returning NULL, which would be the stopping criterion if all requested swaps have been exhausted.
#'
#' @param subgroup_vars Column names of the variables that together define the relevant subgroups
#' @param restrain_on_subgroup_levels Permutations can be forced to take place only within a level of the factor of the subgrouping variable. In this case, the user must pass only one subgrouping variable and a number of levels that together define the permuted subgroup.
#' @param n_swaps Vector with number of swaps to be proposed in successive calls to the returned function (each value should be in valid range from 1..floor(n_locations/2))
#'
#' @return Function to return a list with length n vectors `src` and `dst`, denoting source and destination index for the swap operation, or `NULL` if the user provided a defined protocol for the number of swaps and the last iteration has been reached
#' @export
#'
#' @example man/examples/two_step_optimization.R
mk_subgroup_shuffling_function <- function(subgroup_vars,
                                           restrain_on_subgroup_levels = c(),
                                           n_swaps = 1) {
  force(subgroup_vars)
  force(restrain_on_subgroup_levels)
  force(n_swaps)

  MAX_PERMUTATIONS <- 1e6 # limit memory use of this function

  # Objects that remain in function's name space and will be evaluated on first invocation
  valid_indices <- NULL
  valid_permutations <- NULL

  # suppress no visible binding messages
  src <- dst <- NULL

  # Helper function to analyze batch container and set up valid permutation table on first invocation of shuffling
  setup_perms <- function(batch_container) {
    bc_loc <- batch_container$get_locations()
    assertthat::assert_that(nrow(bc_loc) > 9, msg = "Subgroup shuffling is pointless for small containers (n<10)")
    assertthat::assert_that(all(subgroup_vars %in% colnames(bc_loc)), msg = "All subgroup defining variables have to be part of the container locations")

    assertthat::assert_that(nrow(dplyr::filter(
      dplyr::select(bc_loc, dplyr::all_of(subgroup_vars)),
      dplyr::if_any(dplyr::everything(), ~ !is.na(.))
    )) == nrow(bc_loc),
    msg = "Selected subgrouping variables should not contain any NA values"
    )

    assertthat::assert_that(!(!is.null(restrain_on_subgroup_levels) && length(restrain_on_subgroup_levels) > 0 && length(subgroup_vars) != 1),
      msg = "Exactly one subgrouping variable must be specified if specific subgrouping levels are passed"
    )
    assertthat::assert_that(is.null(restrain_on_subgroup_levels) || length(restrain_on_subgroup_levels) == 0 ||
      all(restrain_on_subgroup_levels %in% bc_loc[[subgroup_vars]]),
    msg = "All selected subgroup levels have to be present in the subgrouping variable"
    )

    if (!is.null(restrain_on_subgroup_levels) && length(restrain_on_subgroup_levels) > 0) { # we focus on selected subgroups only
      valid_indices <<- which(bc_loc[[subgroup_vars]] %in% restrain_on_subgroup_levels)
      subgroup_sizes <- length(valid_indices)
      n_permut <- subgroup_sizes * (subgroup_sizes - 1) / 2
      assertthat::assert_that(n_permut <= MAX_PERMUTATIONS,
        msg = stringr::str_c(
          "Subgroup shuffling would lead to more than ", MAX_PERMUTATIONS,
          " possible permutations. Consider a different solution."
        )
      )
      valid_permutations <<- tidyr::crossing(src = valid_indices, dst = valid_indices) |> dplyr::filter(src < dst)
    } else { # we swap samples across subgroups
      bc_loc <- dplyr::group_by(bc_loc, dplyr::across(dplyr::all_of(subgroup_vars)))
      grp_ind <- dplyr::group_indices(bc_loc)
      subgroup_sizes <- dplyr::group_size(bc_loc)
      n_permut <- sum(subgroup_sizes * (subgroup_sizes - 1) / 2)
      assertthat::assert_that(n_permut <= MAX_PERMUTATIONS,
        msg = stringr::str_c(
          "Subgroup shuffling would lead to more than ", MAX_PERMUTATIONS,
          " possible permutations. Consider a different solution."
        )
      )
      assertthat::assert_that(length(subgroup_sizes) > 1, msg = "Subgroup shuffling is pointless if there's only one subgroup involved")
      valid_permutations <<- purrr::map(seq_along(subgroup_sizes), ~ which(grp_ind == .x)) |>
        purrr::map(~ tidyr::crossing(src = .x, dst = .x) |> dplyr::filter(src < dst)) |>
        dplyr::bind_rows()
    }

    assertthat::assert_that(all(subgroup_sizes > 1), msg = "Subgroup shuffling requires all subgroups to have a minimum size of 2")

    assertthat::assert_that(n_permut == nrow(valid_permutations), msg = "Permutation calculations screwed up. Check the code.")

    valid_indices <<- 1:n_permut

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
  }

  # Helper function to pick n INDEPENDENT permutations, i.e. permutations that don't lead to sample loss
  pick_indep_perm <- function(n) {
    # Start with an index of all possible permutations
    poss_perm <- valid_permutations
    source <- desti <- integer(n)
    for (i in 1:n) {
      p <- floor(stats::runif(1, 1, nrow(poss_perm) + 1))
      source[i] <- poss_perm[["src"]][p]
      desti[i] <- poss_perm[["dst"]][p]
      poss_perm <- dplyr::filter(poss_perm, src != source[i], src != desti[i], dst != source[i], dst != desti[i])
      if (nrow(poss_perm) == 0) {
        # Stop if we don't have any independent exchanges left
        break
      }
    }
    list(src = c(source[1:i], desti[1:i]), dst = c(desti[1:i], source[1:i]))
  }


  if (length(n_swaps) == 1) {
    return(
      function(bc, ...) { # iteration param not used

        if (is.null(valid_permutations)) { # first call
          setup_perms(bc)
        }
        if (n_swaps == 1) {
          swap <- sample(valid_indices, 1)
          return(list(
            src = c(valid_permutations[["src"]][swap], valid_permutations[["dst"]][swap]),
            dst = c(valid_permutations[["dst"]][swap], valid_permutations[["src"]][swap])
          ))
        } else {
          return(pick_indep_perm(n_swaps))
        }
      }
    )
  }

  function(bc, iteration) {
    if (iteration > length(n_swaps)) {
      return(NULL)
    }

    if (is.null(valid_permutations)) { # first call
      setup_perms(bc)
    }

    if (n_swaps[iteration] == 1) {
      swap <- sample(valid_indices, 1)
      return(list(
        src = c(valid_permutations[["src"]][swap], valid_permutations[["dst"]][swap]),
        dst = c(valid_permutations[["dst"]][swap], valid_permutations[["src"]][swap])
      ))
    } else {
      return(pick_indep_perm(n_swaps[iteration]))
    }
  }
}
