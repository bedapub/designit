#' Extract relevant parameters from a generic shuffle function output
#'
#' Any shuffling function should return one of the following:
#' 1. atomic index vector for a direct location assignment
#' 2. a list with src and dst vector
#' 3. a list with locations vector (for location assignment) and optional sample_attr data frame/tibble
#'
#' This function parses the output, performs a few checks and returns results in a simple-to-use list.
#'
#' @param shuffle Return value of a shuffle function
#' @param attributes_expected Logical; if TRUE, sample attributes are expected from the shuffling result and the
#' function dies if they are not provided.
#'
#' @return A list with components src, dst, location_assignment and samples_attr, depending on the output
#' of the specific shuffling function
#' @keywords internal
extract_shuffle_params <- function(shuffle, attributes_expected) {

  # Extracts relevant parameters from shuffle function output and monitors correctness/consistency
  # Tried to avoid redundant checks that are performed on batch container level

  if (is.null(shuffle)) { # marks end of iteration schedule
    return(NULL)
  }

  if (rlang::is_atomic(shuffle)) {
    loc <- shuffle
    src <- dst <- attrib <- NULL
    assertthat::assert_that(!attributes_expected,
      msg = "sample attributes must be consistently supplied by shuffle function once started"
    )
  } else {
    assertthat::assert_that(is.list(shuffle), msg = "shuffle proposal function must return either a numeric vector or a list")
    if (!is.null(shuffle[["src"]]) && !is.null(shuffle[["dst"]])) {
      loc <- NULL
      src <- shuffle[["src"]]
      dst <- shuffle[["dst"]]
    } else {
      assertthat::assert_that(!is.null(shuffle[["location_assignment"]]), msg = "shuffle function must return either a src/dst pair or a location vector")
      loc <- shuffle[["location_assignment"]]
      src <- dst <- NULL
    }
    if (is.null(shuffle[["samples_attr"]])) {
      assertthat::assert_that(!attributes_expected,
        msg = "sample attributes must be consistently supplied by shuffle function once started"
      )
      attrib <- NULL
    } else {
      attrib <- shuffle[["samples_attr"]]
    }
  }

  list(src = src, dst = dst, location_assignment = loc, samples_attr = attrib)
}


#' Helper function to print out one set of scores plus (if needed) aggregated values
#'
#' @param score Vector of (non-aggregated) scores (may be a single value as well)
#' @param agg_score Vector of aggregated scores (may be a single value as well)
#' @param iteration Iteration number
#'
#' @keywords internal
report_scores <- function(score, agg_score, iteration) {
  if (length(score) == 1) {
    if (identical(score, agg_score)) {
      message(
        ifelse(iteration == 0, "Initial", "Achieved"), " score: ", round(score, 3),
        ifelse(iteration == 0, "", stringr::str_c(" at iteration ", iteration))
      )
    } else {
      message(
        ifelse(iteration == 0, "Initial", "Achieved"), " score: ", round(score, 3),
        ifelse(iteration == 0, "", stringr::str_c(" at iteration ", iteration)),
        "   Aggregated: ", round(agg_score, 3)
      )
    }
  } else {
    message(
      ifelse(iteration == 0, "Initial", "Achieved"), " score: ",
      stringr::str_c("c(", stringr::str_c(round(score, 3), collapse = ", "), ")"),
      ifelse(iteration == 0, "", stringr::str_c(" at iteration ", iteration))
    )
    if (!identical(score, agg_score)) {
      if (length(agg_score) == 1) {
        message("   Aggregated: ", round(agg_score, 3))
      } else {
        message("   Aggregated: ", stringr::str_c(" c(", stringr::str_c(round(agg_score, 3), collapse = ", "), ")"))
      }
    }
  }
}

#' Updates a batch container by permuting samples according to a shuffling
#'
#' As post-condition, the batch container is in a different state
#'
#' @param bc The batch container to operate on.
#' @param shuffle_params Shuffling parameters as returned by [extract_shuffle_params()].
#'
#' @return TRUE if sample attributes have been assigned, FALSE otherwise
#'
#' @keywords internal
update_batchcontainer <- function(bc, shuffle_params) {
  bc$move_samples(
    src = shuffle_params$src, dst = shuffle_params$dst,
    location_assignment = shuffle_params$location_assignment
  )

  # Add sample attributes to container if necessary
  if (!is.null(shuffle_params[["samples_attr"]])) {
    bc$samples_attr <- shuffle_params[["samples_attr"]]
    TRUE
  } else {
    FALSE
  }
}



#' Generic optimizer that can be customized by user provided functions for generating shuffles and progressing towards the minimal score
#'
#' @param batch_container An instance of `BatchContainer`.
#' @param samples A `data.frame` with sample information.
#' Should be `NULL` if the `BatchContainer` already has samples in it.
#' @param scoring Scoring function or a named [list()] of scoring functions.
#' @param n_shuffle Vector of length 1 or larger, defining how many random sample
#' swaps should be performed in each iteration. If `length(n_shuffle)==1`,
#' this sets no limit to the number of iterations. Otherwise, the optimization
#' stops if the swapping protocol is exhausted.
#' @param shuffle_proposal_func A user defined function to propose the next shuffling of samples.
#' Takes priority over n_shuffle if both are provided. The function is called with
#' a [BatchContainer] `bc` and an integer parameter `iteration` for the current iteration number,
#' allowing very flexible shuffling strategies.
#' Mapper syntax is supported (see [purrr::as_mapper()]).
#' The returned function must either return a list with fields `src`and `dst` (for pairwise sample swapping)
#' or a numeric vector with a complete re-assigned sample order.
#' @param acceptance_func Alternative function to select a new score as the best one.
#' Defaults to strict improvement rule, i.e. all elements of a score have to be smaller or equal in order to accept the solution as better.
#' This may be replaced with an alternative acceptance function included in the package
#' (e.g. [mk_simanneal_acceptance_func()]) or a user provided function.
#' Mapper syntax is supported (see [purrr::as_mapper()]).
#' @param aggregate_scores_func A function to aggregate multiple scores AFTER (potential) auto-scaling and BEFORE acceptance evaluation.
#' If a function is passed, (multi-dimensional) scores will be transformed (often to a single double value) before calling the acceptance function.
#' E.g., see [first_score_only()] or [worst_score()].
#' Note that particular acceptance functions may require aggregation of a score to a single scalar in order to work, see for example those
#' generated by [mk_simanneal_acceptance_func()].
#' Mapper syntax is supported (see [purrr::as_mapper()]).
#' @param check_score_variance Logical: if TRUE, scores will be checked for variability under sample permutation
#' and the optimization is not performed if at least one subscore appears to have a zero variance.
#' @param autoscale_scores Logical: if TRUE, perform a transformation on the fly to equally scale scores
#' to a standard normal. This makes scores more directly comparable and easier to aggregate.
#' @param autoscaling_permutations How many random sample permutations should be done to estimate autoscaling parameters.
#' (Note: minimum will be 20, regardless of the specified value)
#' @param autoscale_useboxcox Logical; if TRUE, use a boxcox transformation for the autoscaling if possible at all.
#' Requires installation of the `bestNormalize` package.
#' @param sample_attributes_fixed Logical; if TRUE, sample shuffle function may generate altered sample attributes at each iteration.
#' This affects estimation of score distributions. (Parameter only relevant if shuffle function does introduce attributes!)
#' @param max_iter Stop optimization after a maximum number of iterations,
#' independent from other stopping criteria (user defined shuffle proposal or min_delta).
#' @param min_delta If not NA, optimization is stopped as soon as successive improvement (i.e. euclidean distance between score vectors
#' from current best and previously best solution) drops below min_delta.
#' @param quiet If TRUE, suppress non-critical warnings or messages.
#'
#' @return A trace object
#'
#' @export
#'
#' @examples
#' data("invivo_study_samples")
#' bc <- BatchContainer$new(
#'   dimensions = c("plate" = 2, "column" = 5, "row" = 6)
#' )
#' bc <- optimize_design(bc, invivo_study_samples,
#'   scoring = osat_score_generator("plate", "Sex"),
#'   max_iter = 100
#' )
#' plot_plate(bc$get_samples(), .col = Sex)
optimize_design <- function(batch_container, samples = NULL,
                            scoring = NULL,
                            n_shuffle = NULL,
                            shuffle_proposal_func = NULL,
                            acceptance_func = accept_strict_improvement,
                            aggregate_scores_func = identity,
                            check_score_variance = TRUE,
                            autoscale_scores = FALSE, autoscaling_permutations = 100, autoscale_useboxcox = TRUE,
                            sample_attributes_fixed = FALSE,
                            max_iter = 1e4, min_delta = NA, quiet = FALSE) {
  start_time <- Sys.time()
  cl <- match.call()

  # create a copy, so that we do not modify the BatchContainer
  batch_container <- batch_container$copy()
  trace <- tibble::tibble(
    optimization_index = max(batch_container$trace$optimization_index, 0) + 1,
    call = list(cl),
    start_assignment_vec = list(batch_container$assignment)
  )

  # saving the current random seed for reproducibility
  if (!exists(".Random.seed", .GlobalEnv)) {
    # Sets the default random number generator and initializes
    # .Random.seed in the global environment.
    # This doesn't affect the way random seed would have been set in
    # downstream functions such as `sample()`.
    set.seed(NULL)
  }
  trace$seed <- list(.Random.seed)
  trace$rng_kind <- list(RNGkind())

  if (is.null(samples)) {
    assertthat::assert_that(batch_container$has_samples,
      msg = "batch-container is empty and no samples provided"
    )
  } else {
    assertthat::assert_that(nrow(samples) > 0)
    batch_container <- assign_in_order(batch_container, samples)
  }


  assertthat::assert_that(
    !is.null(scoring),
    msg = "Scoring should be provided when calling optimize_design()"
  )

  # Get assigned samples and locations from the batch container
  samp <- batch_container$get_samples(include_id = TRUE, assignment = TRUE, remove_empty_locations = FALSE)
  n_samples <- length(stats::na.exclude(samp$.sample_id))
  n_locations <- nrow(samp)

  assertthat::assert_that(".sample_id" %in% colnames(samp),
    all(sort(samp$.sample_id, na.last = NA) == 1:n_samples),
    msg = stringr::str_c(".sample_id from batch container must exist and numerate samples from 1 to ", n_samples)
  )

  assertthat::assert_that(is.null(n_shuffle) ||
    (all(rlang::is_integerish(n_shuffle, finite = TRUE)) && all(n_shuffle >= 1)),
  msg = "n_shuffle should be an integer or an integer vector (>=1), or NULL"
  )


  # Create shuffle_proposal_func
  # If passed by the user, this one getting priority over n_shuffle.
  # If nothing is passed, default shuffling function is to swap 2 random elements per iteration, which
  # is implemented by an especially efficient function.
  if (is.null(n_shuffle) && is.null(shuffle_proposal_func)) {
    shuffle_proposal_func <- mk_swapping_function(n_swaps = 1)
  } else if (is.null(shuffle_proposal_func)) {
    shuffle_proposal_func <- mk_swapping_function(n_swaps = n_shuffle)
    if (length(n_shuffle) > 1) {
      # Restrict number if iters, so that also trace object will be appropriately sized
      max_iter <- min(max_iter, length(n_shuffle), na.rm = T)
    }
  }
  shuffle_proposal_func <- rlang::as_function(shuffle_proposal_func)

  assertthat::assert_that(is.function(shuffle_proposal_func), msg = "shuffle_proposal_func should be a function")

  aggregate_scores_func <- rlang::as_function(aggregate_scores_func)
  acceptance_func <- rlang::as_function(acceptance_func)


  # Do first iteration outside of loop; helps to perform initial checks just once
  iteration <- 1
  using_attributes <- FALSE # keeps track if attributes had been used in 1st iteration, since they must be provided consistently

  shuffle_params <- shuffle_proposal_func(batch_container, iteration) |>
    extract_shuffle_params(attributes_expected = FALSE)

  # Always perform first shuffling before scoring the bc; works also if attributes are added at this stage
  # Otherwise remember initial state as currently best shuffling
  if (!is.null(shuffle_params[["samples_attr"]])) {
    if (!quiet) message("Usage of sample attributes --> executing first shuffling call.")
    update_batchcontainer(batch_container, shuffle_params)
    best_shuffle <- shuffle_params
  } else {
    best_shuffle <- list(src = NULL, dst = NULL, location_assignment = batch_container$assignment, samples_attr = NULL)
  }

  # Evaluate this just once in order not to break current tests
  initial_score <- batch_container$score(scoring)
  score_dim <- length(initial_score)

  # Check score variances (should be all >0)
  if (check_score_variance) {
    bc_copy <- batch_container$copy()
    score_vars <- random_score_variances(batch_container$copy(), scoring = scoring, random_perm = 100, sample_attributes_fixed)
    low_var_scores <- score_vars < 1e-10
    if (!quiet) {
      message(
        "Checking variances of ", length(low_var_scores), "-dim. score vector.",
        "\n... (", stringr::str_c(round(score_vars, 3), collapse = ", "), ")",
        ifelse(any(low_var_scores), " !!", " - OK")
      )
    }
    ne_scores <- assertthat::validate_that(!any(is.na(low_var_scores)),
      msg = stringr::str_c(
        "Caution! Non-evaluable scores detected! Check scores # ",
        stringr::str_c(which(is.na(low_var_scores)), collapse = ", ")
      )
    )
    if (is.character(ne_scores)) message(ne_scores)
    assertthat::assert_that(!any(low_var_scores),
      msg = stringr::str_c(
        "Low variance scores detected! Check scores # ",
        stringr::str_c(which(low_var_scores), collapse = ", ")
      )
    )
  }


  # Set up autoscaling function if needed
  if (autoscale_scores && score_dim > 1) {
    autoscaling_permutations <- max(20, floor(autoscaling_permutations))
    if (!quiet) {
      message(
        "Creating autoscaling function for ", score_dim, "-dim. score vector. (",
        autoscaling_permutations, " random permutations)"
      )
    }
    autoscale_func <- mk_autoscale_function(batch_container$copy(),
      scoring = scoring,
      random_perm = autoscaling_permutations,
      use_boxcox = autoscale_useboxcox,
      sample_attributes_fixed
    )
  } else {
    autoscale_func <- identity
  }

  # Calculate and remember initial multi-variate & aggregated score
  best_score <- autoscale_func(initial_score)
  best_agg <- aggregate_scores_func(best_score)
  prev_agg <- NULL


  scores_mat <- matrix(
    nrow = max_iter + 1, # + 1 to accommodate initial score
    ncol = length(best_score),
    dimnames = list(NULL, names(best_score))
  )

  scores_mat[1,] <- best_score
  if (identical(aggregate_scores_func, identity)) {
    aggregated_scores_mat <- NULL
  } else {
    aggregated_scores_mat <- matrix(
      nrow = max_iter + 1, # + 1 to accommodate initial score
      ncol = length(best_agg),
      dimnames = list(NULL, names(best_agg))
    )
    aggregated_scores_mat[1,] <- best_agg
  }

  if (!quiet) report_scores(best_score, best_agg, iteration = 0)


  while (!is.null(shuffle_params) && (iteration <= max_iter)) { # NULL may indicate end of permutation protocol

    attribs_assigned <- update_batchcontainer(batch_container, shuffle_params)
    if (!using_attributes && attribs_assigned) {
      message("Adding ", ncol(shuffle_params[["samples_attr"]]), " attributes to samples.")
      using_attributes <- TRUE
    }

    new_score <- autoscale_func(batch_container$score(scoring))
    assertthat::assert_that(!any(is.na(new_score)), msg = stringr::str_c("NA apprearing during scoring in iteration ", iteration))
    new_agg <- aggregate_scores_func(new_score)

    if (acceptance_func(new_agg, best_agg, iteration = iteration)) {
      best_score <- new_score
      prev_agg <- best_agg
      best_agg <- new_agg
      best_shuffle <- list(
        src = NULL, dst = NULL, location_assignment = batch_container$assignment,
        samples_attr = shuffle_params[["samples_attr"]]
      )
      if (!quiet) report_scores(best_score, best_agg, iteration)
    } else {
      if (is.null(shuffle_params[["location_assignment"]])) { # we used the permutation method and thus have to swap samples back!
        update_batchcontainer(batch_container, shuffle_params) # two swaps are in effect no swaps
      }
    }

    iteration <- iteration + 1
    scores_mat[iteration,] <- best_score
    if (!is.null(aggregated_scores_mat)) {
      aggregated_scores_mat[iteration,] <- best_agg
    }

    # Test stopping criteria
    if (!is.na(min_delta) && !is.null(prev_agg)) {
      delta <- sqrt(sum((best_agg - prev_agg)^2))
      if (delta < min_delta) {
        if (!quiet) message("Reached min delta in ", iteration - 1, " iterations.")
        break
      }
    }

    if (iteration <= max_iter) {
      # only call shuffle_proposal_func in case we have more iterations
      shuffle_params <- shuffle_proposal_func(batch_container, iteration) |>
        extract_shuffle_params(attributes_expected = using_attributes)
    }
  }

  # In the end, always make sure that final state of bc is the one with the best score
  update_batchcontainer(batch_container, best_shuffle)

  # shrink
  trace$scores <- shrink_mat(scores_mat, iteration)
  trace$aggregated_scores <- shrink_mat(aggregated_scores_mat, iteration)
  trace$elapsed <- Sys.time() - start_time
  trace$end_assignment_vec = list(batch_container$assignment)
  batch_container$trace <- dplyr::bind_rows(
    batch_container$trace,
    trace
  )
  batch_container
}
