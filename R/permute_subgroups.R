


#' Form groups and subgroups of 'homogeneous' samples as defined by certain variables and size constraints
#'
#' @param batch_container Batch container with all samples assigned that are to be grouped and sub-grouped
#' @param allocate_var Name of a variable in the `samples` table to inform possible groupings, as (sub)group sizes must add up to the correct totals
#' @param keep_together_vars Vector of column names in sample table; groups are formed by pooling samples with identical values of all those variables
#' @param n_min Minimal number of samples in one sub(!)group; by default 1
#' @param n_max Maximal number of samples in one sub(!)group; by default the size of the biggest group
#' @param n_ideal Ideal number of samples in one sub(!)group; by default the floor or ceiling of `mean(n_min,n_max)`, depending on the setting of `prefer_big_groups`
#' @param subgroup_var_name An optional column name for the subgroups which are formed (or NULL)
#' @param prefer_big_groups Boolean; indicating whether or not bigger subgroups should be preferred in case of several possibilities
#' @param strict Boolean; if TRUE, subgroup size constraints have to be met strictly, implying the possibility of finding no solution at all
#'
#' @return Subgroup object to be used in subsequent calls to `compile_possible_subgroup_allocation()`
#' @export
#'
#' @examples
form_homogeneous_subgroups <- function(batch_container, allocate_var, keep_together_vars = c(),
                                       n_min = NA, n_max = NA, n_ideal = NA, subgroup_var_name = NULL,
                                       prefer_big_groups = TRUE, strict = TRUE) {
  assertthat::assert_that(batch_container$has_samples, msg = "Batch container must have samples assigned.")
  # Don't look at potential empty locations in the bc when determining group sizes
  samples <- batch_container$get_samples(assignment = TRUE, remove_empty_locations = TRUE)

  # Check all vital arguments
  assertthat::assert_that(nrow(samples) > 1, msg = "Sample list must contain at least 2 samples.")

  assertthat::assert_that(allocate_var %in% colnames(samples), msg = "Allocation variable not found in sample table.")
  allocate_fac <- samples[[allocate_var]]
  assertthat::assert_that(!any(is.null(allocate_fac) | is.na(allocate_fac) | is.nan(allocate_fac)),
    msg = "No undefined/empty levels of the allocation variable are allowed."
  )
  # The allocation variable must have factor levels in a given order!
  # This is important later on for sample swapping as it has to be known which factor level corresponds to 'group 1' etc
  if (!inherits(allocate_fac, "factor")) {
    allocate_fac <- factor(allocate_fac, levels = unique(allocate_fac))
  }

  assertthat::assert_that(length(keep_together_vars) > 0, msg = "Function can only help if vector of 'keep_together_vars' is specified.")


  use_vars <- intersect(keep_together_vars, colnames(samples))
  assertthat::assert_that(length(use_vars) > 0, msg = "No overlap between 'keep_together_vars' and columns in sample table.")

  if (length(use_vars) < length(keep_together_vars)) {
    warning(
      "Ignoring 'keep_together_vars' not found in sample table: ",
      stringr::str_c(setdiff(keep_together_vars, use_vars), collapse = ", ")
    )
  }

  # Set proper values for the subgroup sizes
  if (is.na(n_min)) {
    n_min <- 1
  }
  if (is.na(n_max)) {
    n_max <- max(summary(allocate_fac))
  }
  assertthat::assert_that(n_min <= n_max, msg = stringr::str_c("n_min (", n_min, ") must not be greater than n_max (", n_max, ")."))

  # Select n_ideal if not specified from given min and max
  if (is.na(n_ideal)) {
    n_ideal <- if (prefer_big_groups) ceiling((n_min + n_max) / 2) else floor((n_min + n_max) / 2)
  }
  assertthat::assert_that(n_ideal <= n_max, n_ideal >= n_min,
    msg = stringr::str_c("n_ideal (", n_ideal, ") must be between n_min (", n_min, ") and n_max (", n_max, ").")
  )

  best_group_sizes <- function(n, nmin, nmax, nideal, prefer_big) {
    if (n <= nmin) { # don't split at all if n already below minimum
      return(n)
    }
    if (n %% nideal == 0) { # best case: group size is dividable by n_ideal
      return(rep.int(nideal, n / nideal))
    }

    # Number of ideal sized groups, depending of remaining samples form an own group with >=nmin samples
    n_ideal_groups <- floor(n / nideal) - (n %% nideal < nmin)
    remain <- n - n_ideal_groups * nideal

    # Which possible group size should be taken to split the remaining samples?
    size_fits <- which(remain %% (1:remain) == 0)
    perfect_sizes <- size_fits[size_fits >= nmin & size_fits <= nmax]
    if (any(perfect_sizes)) { # can solve the remaining problem with identical splits of allowed size
      if (prefer_big && any(perfect_sizes > nideal)) {
        size_remainder <- min(perfect_sizes[perfect_sizes > nideal])
      } else if (!prefer_big && any(perfect_sizes <= nideal)) {
        size_remainder <- max(perfect_sizes[perfect_sizes <= nideal])
      } else {
        size_remainder <- if (prefer_big) max(perfect_sizes) else min(perfect_sizes)
      }
      size_remainder <- if (prefer_big) max(perfect_sizes) else min(perfect_sizes)
      return(c(rep.int(nideal, n_ideal_groups), rep.int(size_remainder, remain / size_remainder)))
    }

    # Cannot split remains in equal groups sizes within valid range; try with smaller value of n_ideal
    nideal <- nideal - 1
    if (nideal < nmin) {
      nmin <- nideal
    }
    c(rep.int(nideal, n_ideal_groups), best_group_sizes(remain, nmin, nmax, nideal, prefer_big = T))
  }

  # Group sample list by relevant variables
  grouped_samples <- dplyr::group_by(samples, dplyr::across(dplyr::all_of(use_vars)))

  # Determine sizes of the subgroups and store in list; name elements by levels of the involved grouping variables
  subgroup_sizes <- purrr::map(dplyr::group_size(grouped_samples), ~ best_group_sizes(.x, n_min, n_max, n_ideal, prefer_big_groups))
  names(subgroup_sizes) <- dplyr::group_keys(grouped_samples) |>
    tidyr::unite(col = "keys", sep = "/") |>
    dplyr::pull(1)

  assertthat::assert_that(!strict || (min(unlist(subgroup_sizes)) >= n_min && max(unlist(subgroup_sizes)) <= n_max),
    msg = "Cannot form subgroups under strict setting with given constraints!"
  )

  message(
    "\nFormed ", dplyr::n_groups(grouped_samples), " homogeneous groups using ", nrow(grouped_samples), " samples.\n",
    length(unlist(subgroup_sizes)), " subgroups needed to satisfy size constraints."
  )

  list(
    Grouped_Samples = grouped_samples, Subgroup_Sizes = subgroup_sizes,
    Allocate_Var = allocate_var, Allocate_Levels = levels(allocate_fac),
    Subgroup_Var_Name = subgroup_var_name,
    options = list(
      n_min = n_min, n_max = n_max, n_ideal = n_ideal,
      prefer_big_groups = prefer_big_groups,
      strict = strict
    )
  )
}



#' Validate subgroup object and stop with error message if not all required fields are there
#'
#' @param subgroup_object Subgrouping object as returned by `form_homogeneous_subgroups()`
#'
#' @keywords internal
validate_subgrouping_object <- function(subgroup_object) {
  assertthat::assert_that(all(c("Grouped_Samples", "Subgroup_Sizes", "Allocate_Var", "Subgroup_Var_Name", "Allocate_Levels")
  %in% names(subgroup_object)),
  msg = "Invalid subgroup object passed."
  )
}



#' Internal function to generate possible subgroup combinations that add up to specific levels of an allocation variable
#'
#' @param block_sizes (Integer) vector of sizes of the various subgroups that can be combined to form groups
#' @param group_nums Vector of sizes of the different groups to be formed
#' @param fullTree Boolean: Enforce full search of the possibility tree, independent of the value of `maxCalls`
#' @param maxCalls Maximum number of recursive calls in the search tree, to avoid long run times with very large trees
#'
#' @return List of possible allocations; Each allocation is an integer vector of allocation levels that are assigned in that order to the subgroups with sizes given by `block sizes`
#' @keywords internal
find_possible_block_allocations <- function(block_sizes, group_nums, fullTree = FALSE, maxCalls = 1e6) {
  allocs <- list()
  ncalls <- 0

  message(
    "\nFinding possible ways to allocate variable of interest with ",
    length(group_nums), " levels ..."
  )

  find_alloc <- function(done_groups, todo_blocks, groups_left) {
    ncalls <<- ncalls + 1
    if (length(todo_blocks) == 1 && sum(groups_left > 0) == 1 &&
      groups_left[groups_left > 0] == todo_blocks) {
      allocs[[length(allocs) + 1]] <<- unname(c(done_groups, which(groups_left > 0)))
    } else if (fullTree || ncalls < maxCalls) {
      to_try <- which(todo_blocks[1] <= groups_left)
      for (i in to_try) {
        tmp <- groups_left
        tmp[i] <- tmp[i] - todo_blocks[1]
        find_alloc(
          done_groups = c(done_groups, i),
          todo_blocks = todo_blocks[-1],
          groups_left = tmp
        )
      }
    }
  }

  if (sum(group_nums) > sum(block_sizes)) {
    warning("More group allocations requested than in available blocks. No solution returned.")
  } else if (sum(group_nums) < sum(block_sizes)) {
    warning("Surplus block members. All available units are to be allocated. No solution returned.")
  } else {
    find_alloc(done_groups = c(), todo_blocks = block_sizes, groups_left = group_nums)
    if (ncalls >= maxCalls && !fullTree) {
      message("\nAborted after ", ncalls, " recursive calls (maxCalls limit).\n", length(allocs), " allocations found.")
    } else {
      message("\nFinished with ", ncalls, " recursive calls.\n", length(allocs), " allocations found.")
    }
  }

  allocs
}


#' Compile list of all possible ways to assign levels of the allocation variable to a given set of subgroups
#'
#' All information needed to perform this function (primarily the number and size of subgroups plus the levels of the
#' allocation variable) are contained in and extracted from the subgroup object.
#'
#' @param subgroup_object A subgrouping object as returned by `form_homogeneous_subgroups()`
#' @param fullTree Boolean: Enforce full search of the possibility tree, independent of the value of `maxCalls`
#' @param maxCalls Maximum number of recursive calls in the search tree, to avoid long run times with very large trees
#'
#' @return List of possible allocations; Each allocation is an integer vector of allocation levels that are assigned in that order to the subgroups with given sizes
#' @export
#'
#' @examples
compile_possible_subgroup_allocation <- function(subgroup_object, fullTree = FALSE, maxCalls = 1e6) {
  validate_subgrouping_object(subgroup_object)

  find_possible_block_allocations(unlist(subgroup_object$Subgroup_Sizes, use.names = F),
    table(factor(subgroup_object$Grouped_Samples[[subgroup_object$Allocate_Var]],
      levels = subgroup_object$Allocate_Levels
    )),
    fullTree = fullTree,
    maxCalls = maxCalls
  )
}


#' Compose shuffling function based on already available subgrouping and allocation information
#'
#' @param subgroup_object A subgrouping object as returned by `form_homogeneous_subgroups()`
#' @param subgroup_allocations A list of possible assignments of the allocation variable as returned by `compile_possible_subgroup_allocation()`
#' @param keep_separate_vars Vector of column names in sample table; items with identical values in those variables will not be put into the same subgroup if at all possible
#' @param report_grouping_as_attribute Boolean, if TRUE, add an attribute table to the permutation functions' output, to be used in scoring during the design optimization
#'
#' @return Shuffling function that on each call returns an index vector for a valid sample permutation
#' @export
#'
#' @examples
shuffle_with_subgroup_formation <- function(subgroup_object, subgroup_allocations,
                                            keep_separate_vars = c(),
                                            report_grouping_as_attribute = FALSE) {
  force(subgroup_object)
  force(subgroup_allocations)
  force(keep_separate_vars)
  force(report_grouping_as_attribute)

  validate_subgrouping_object(subgroup_object)

  # Custom implementation of sample which deals with vectors of length 1 properly
  my_sample <- function(x) {
    if (length(x) == 1) {
      return(x)
    }
    base::sample(x)
  }

  if (length(keep_separate_vars) > 0) {
    sep_vars <- intersect(keep_separate_vars, colnames(subgroup_object$Grouped_Samples))
    if (length(sep_vars) == 0) {
      warning("No overlap between 'keep_separate_vars' and columns in sample table --> ignored.")
      sep_vars <- NULL
    } else if (length(sep_vars) < length(keep_separate_vars)) {
      warning(
        "Ignoring 'keep_separate_vars' not found in sample table: ",
        paste0(setdiff(keep_separate_vars, sep_vars), collapse = ", ")
      )
    }
  } else {
    sep_vars <- NULL
  }

  # Explicitly spell out the different possible subgroup allocations on a sample level
  alloc_vectors <- purrr::map(subgroup_allocations, rep.int, times = unlist(subgroup_object$Subgroup_Sizes, use.names = FALSE))

  # Helper vector for the group level structure in the sample space
  # Note that this is ordered by the logical structure of the grouping, not the order of samples!
  group_vec <- rep.int(
    seq_along(subgroup_object$Subgroup_Sizes),
    purrr::map_dbl(subgroup_object$Subgroup_Sizes, sum)
  )

  # Helper vector for the subgroup level structure;
  # Note that this is ordered by the logical structure of the (sub)grouping, not the order of samples!
  subgroup_vec <- rep.int(
    unlist(purrr::map(subgroup_object$Subgroup_Sizes, seq_along), use.names = FALSE),
    unlist(subgroup_object$Subgroup_Sizes, use.names = FALSE)
  )

  fullgroup_vec <- stringr::str_c(group_vec, subgroup_vec, sep = "_")

  # Index vector to assign group allocation to sample dataframe in its original order.
  # Split up according to group structure to allow easy permutation on the subgroup level
  sample_vec <- order(dplyr::group_indices(subgroup_object$Grouped_Samples)) |>
    split(f = group_vec)

  # Order of the allocation variable in the batch container may be arbitrary and not strictly sorted;
  # Since this column won't be permuted along with the samples, we have to build an index for mapping
  # sorted logical group information back into the batch container
  alloc_var_orig <- factor(subgroup_object$Grouped_Samples[[subgroup_object$Allocate_Var]],
    levels = subgroup_object$Allocate_Levels
  )
  alloc_var_order <- order(alloc_var_orig)


  idx <- 0 # index of currently selected allocation

  sep_fails_tolerance <- 0 # number of violations allowed for the keep_separate_variable constraint

  # Static memory for the actual returned allocation
  shuffle_index <- integer(length(subgroup_vec))
  alloc_var_assigned <- integer(length(subgroup_vec))
  group_labels <- integer(length(group_vec))
  subgroup_labels <- integer(length(subgroup_vec))
  fullgroup_labels <- character(length(subgroup_vec))


  # Function to return one shuffle proposal for the sample list in its original order
  # Values refer to the levels of the allocation variable
  # All constraints are guaranteed to be satisfied

  function(...) {
    idx <<- idx + 1
    if (idx > length(alloc_vectors)) {
      idx <<- 1
    }

    # Create a random permutation within (!) all the groups
    # This means that only 'equivalent' items are swapped around and get assigned to new subgroups every time
    rand_index <- purrr::map(sample_vec, my_sample) |> unlist(use.names = F)

    # Check if keep_separate_vars constraints are fulfilled within the randomized subgroups
    # Otherwise, create new permutations and increment number of tolerated violations every 1000 unsuccessful attempts
    if (!is.null(sep_vars)) {
      fails <- 0
      repeat {
        dups <- purrr::map_int(sep_vars, ~ sum(duplicated(cbind(subgroup_object$Grouped_Samples[rand_index, .x], fullgroup_vec))))
        if (max(dups) <= sep_fails_tolerance) {
          break
        }
        fails <- fails + 1
        if (fails > 1000) {
          fails <- 0
          sep_fails_tolerance <<- sep_fails_tolerance + 1
          message(
            "No permutations fulfilling the 'keep_separate' constraints in 1000 iters!\n",
            "Increasing number of tolerated violations to ", sep_fails_tolerance
          )
        }
        rand_index <- purrr::map(sample_vec, my_sample) |> unlist(use.names = F)
      }
    }


    # Fill the values for a concrete sample permutation that justifies the constraints.
    # The following three vectors could be bound to the sample in their original order to denote the group assignment
    alloc_var_assigned[rand_index] <<- alloc_vectors[[idx]] # the allocated variable
    group_labels[rand_index] <<- group_vec # the group information
    subgroup_labels[rand_index] <<- subgroup_vec # the subgroup information
    fullgroup_labels[rand_index] <<- fullgroup_vec # both joined

    grouped_order <- order(alloc_var_assigned, group_labels, subgroup_labels) # indices to bring orig. sample list into group based order

    shuffle_index <<- grouped_order[alloc_var_order] # bring into order determined by the allocation variable in the batch container

    if (!report_grouping_as_attribute) {
      return(shuffle_index)
    }

    attrib <- tibble::tibble(
      alloc_var_level = subgroup_object$Allocate_Levels[alloc_var_assigned],
      group = group_labels,
      subgroup = subgroup_labels
    )

    if (!is.null(subgroup_object$Subgroup_Var_Name)) {
      attrib[[subgroup_object$Subgroup_Var_Name]] <- fullgroup_labels # [shuffle_index]
    }

    list(location_assignment = shuffle_index, samples_attr = attrib)
  }
}


#' Generate in one go a shuffling function that produces permutations with specific constraints on multiple sample variables and group sizes fitting one specific allocation variable
#'
#' @param batch_container Batch container with all samples assigned that are to be grouped and sub-grouped
#' @param allocate_var Name of a variable in the `samples` table to inform possible groupings, as (sub)group sizes must add up to the correct totals
#' @param keep_together_vars Vector of column names in sample table; groups are formed by pooling samples with identical values of all those variables
#' @param keep_separate_vars Vector of column names in sample table; items with identical values in those variables will not be put into the same subgroup if at all possible
#' @param n_min Minimal number of samples in one sub(!)group; by default 1
#' @param n_max Maximal number of samples in one sub(!)group; by default the size of the biggest group
#' @param n_ideal Ideal number of samples in one sub(!)group; by default the floor or ceiling of `mean(n_min,n_max)`, depending on the setting of `prefer_big_groups`
#' @param subgroup_var_name An optional column name for the subgroups which are formed (or NULL)
#' @param report_grouping_as_attribute Boolean, if TRUE, add an attribute table to the permutation functions' output, to be used in scoring during the design optimization
#' @param prefer_big_groups Boolean; indicating whether or not bigger subgroups should be preferred in case of several possibilities
#' @param strict Boolean; if TRUE, subgroup size constraints have to be met strictly, implying the possibility of finding no solution at all
#' @param fullTree Boolean: Enforce full search of the possibility tree, independent of the value of `maxCalls`
#' @param maxCalls Maximum number of recursive calls in the search tree, to avoid long run times with very large trees
#'
#' @return Shuffling function that on each call returns an index vector for a valid sample permutation
#' @export
#'
#' @examples
shuffle_grouped_data <- function(batch_container, allocate_var,
                                 keep_together_vars = c(),
                                 keep_separate_vars = c(),
                                 n_min = NA, n_max = NA, n_ideal = NA,
                                 subgroup_var_name = NULL,
                                 report_grouping_as_attribute = FALSE,
                                 prefer_big_groups = FALSE, strict = TRUE,
                                 fullTree = FALSE, maxCalls = 1e6) {
  sg <- form_homogeneous_subgroups(
    batch_container = batch_container, allocate_var = allocate_var,
    keep_together_vars = keep_together_vars,
    subgroup_var_name = subgroup_var_name,
    n_min = n_min, n_max = n_max, n_ideal = n_ideal,
    prefer_big_groups = prefer_big_groups, strict = strict
  )

  sa <- compile_possible_subgroup_allocation(sg, fullTree = fullTree, maxCalls = maxCalls)

  shuffle_with_subgroup_formation(sg, sa, keep_separate_vars = keep_separate_vars, report_grouping_as_attribute = report_grouping_as_attribute)
}
