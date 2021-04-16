


form_homogeneous_subgroups = function(samples, allocate_var, keep_together_vars=c(),
                                      n_min=NA, n_max=NA, n_ideal=NA, prefer_big_groups=FALSE, strict=TRUE) {

  if (nrow(samples)==0 || length(allocate_var)==0 || nrow(samples)!=length(allocate_var)) {
    stop("'samples' (data frame) and 'allocate_var' (vector) both have to be passed and match in size.")
  }
  if (length(keep_together_vars)==0) {
    stop("function only makes sense if vector of 'keep_together_vars' is specified.")
  }
  use_vars = intersect(keep_together_vars, colnames(samples))
  if (length(use_vars)==0) {
    stop("No overlap between 'keep_together_vars' and columns in sample table.")
  }
  #if (is.na(n_min) && is.na(n_max) && is.na(n_ideal)) {
  #  stop("Have to specify at least one of 'n_min', 'n_max', 'n_ideal'.")
  #}
  if (length(use_vars)<length(keep_together_vars)) {
    warning("Ignoring 'keep_together_vars' not found in sample table: ",
            paste0(setdiff(keep_together_vars, use_vars), collapse=", "))
  }
  if (class(allocate_var)!="factor") {
    allocate_var = factor(allocate_var)
  }

  if (is.na(n_min)) {
    n_min=1
  }
  if (is.na(n_max)) {
    n_max=max(table(allocate_var))
  }
  if (n_min>n_max) {
    stop("n_min (",n_min,") must not be greater than n_max (",n_max,").")
  }
  # Select n_ideal if not specified from given min and max
  if (is.na(n_ideal)) {
    n_ideal = ifelse(prefer_big_groups, ceiling((n_min+n_max)/2), floor((n_min+n_max)/2))
  }
  if (n_ideal>n_max || n_ideal<n_min) {
    stop("n_ideal (", n_ideal, ") must be between n_min (",n_min,") and n_max (",n_max,").")
  }

  best_group_sizes = function(n, nmin, nmax, nideal, prefer_big) {
    if (n<=nmin) { # don't split at all if n already below minimum
      return(n)
    }
    if (n %% nideal==0) { # best case: group size is dividable by n_ideal
      return(rep.int( nideal, n/nideal))
    }

    # Number of ideal sized groups, depending of remaining samples form an own group with >=nmin samples
    n_ideal_groups = floor(n/nideal) - (n %% nideal<nmin)
    remain = n-n_ideal_groups*nideal

    # Which possible group size should be taken to split the remaining samples?
    size_fits = which(remain %% (1:remain)==0)
    perfect_sizes = size_fits[ size_fits>=nmin & size_fits<=nmax]
    if (any(perfect_sizes)) { # can solve the remaining problem with identical splits of allowed size
      if (prefer_big && any(perfect_sizes>nideal)) {
        size_remainder = min(perfect_sizes[perfect_sizes>nideal])
      } else if (!prefer_big && any(perfect_sizes<=nideal)) {
        size_remainder = max(perfect_sizes[perfect_sizes<=nideal])
      } else {
        size_remainder = ifelse(prefer_big, max(perfect_sizes), min(perfect_sizes))
      }
      size_remainder = ifelse(prefer_big, max(perfect_sizes), min(perfect_sizes))
      return( c( rep.int(nideal, n_ideal_groups), rep.int(size_remainder, remain/size_remainder) ) )
    }

    # Cannot split remains in equal groups sizes within valid range; try with smaller value of n_ideal
    nideal=nideal-1
    if (nideal<nmin) {
      nmin=nideal
    }
    c( rep.int(nideal, n_ideal_groups), best_group_sizes(remain,nmin,nmax,nideal,prefer_big = T) )

  }

  # Group sample list by relevant variables
  grouped_samples = dplyr::group_by(samples,across(all_of(use_vars)))

  subgroup_sizes = purrr::map(dplyr::group_size(grouped_samples), ~ best_group_sizes(.x, n_min, n_max, n_ideal, prefer_big_groups))

  if (strict && (min(unlist(subgroup_sizes))<n_min || max(unlist(subgroup_sizes))>n_max)) {
    stop("Cannot form subgroups under strict setting with given constraints!")
  }

  message("\nFormed ",dplyr::n_groups(grouped_samples), " homogeneous groups using ",nrow(grouped_samples)," samples.\n",
          length(unlist(subgroup_sizes))," subgroups needed to satisfy size constraints.")

  list(Grouped_Samples = grouped_samples, Subgroup_Sizes = subgroup_sizes, Allocate_Var = allocate_var,
       options = list( n_min=n_min, n_max=n_max, n_ideal=n_ideal,  prefer_big_groups=prefer_big_groups,
                       strict=strict)
  )
}

find_possible_block_allocations = function(block_sizes, group_nums, fullTree=FALSE, maxCalls=1e6) {

  allocs = list()
  ncalls=0

  message("\nFinding possible ways to allocate variable of interest with ",
          length(group_nums), " levels ...")

  find_alloc = function(done_groups, todo_blocks, groups_left) {
    #cat("\nInvoke with done =  ",done_groups,"\n")
    ncalls<<-ncalls+1
    if (length(todo_blocks)==1 && sum(groups_left>0)==1 &&
        groups_left[groups_left>0]==todo_blocks) {
      #cat("\nFound:", done_groups,which(groups_left>0),"\n","last block:",
      #    todo_blocks,"   left:",groups_left[groups_left>0],"\n")
      #cat("*")
      allocs[[length(allocs)+1]]<<-unname(c(done_groups,which(groups_left>0)))
    } else if (fullTree || ncalls<maxCalls) {
      to_try = which(todo_blocks[1]<=groups_left)
      for (i in to_try) {
        tmp = groups_left
        tmp[i] = tmp[i]-todo_blocks[1]
        find_alloc( done_groups = c(done_groups,i),
                    todo_blocks = todo_blocks[-1],
                    groups_left = tmp)
      }
    }
  }

  if (sum(group_nums)>sum(block_sizes)) {
    warning("More group allocations requested than in available blocks. No solution returned.")
  } else if (sum(group_nums)<sum(block_sizes)) {
    warning("Surplus block members. All available units are to be allocated. No solution returned.")
  } else {
    find_alloc(done_groups=c(), todo_blocks=block_sizes, groups_left=group_nums)
    if (ncalls>=maxCalls && !fullTree) {
      message("\nAborted after ",ncalls," recursive calls (maxCalls limit).\n",length(allocs)," allocations found.")
    } else {
      message("\nFinished with ",ncalls," recursive calls.\n",length(allocs)," allocations found.")
    }
  }

  allocs
}


compile_possible_subgroup_allocation = function(subgroup_object, fullTree=FALSE, maxCalls=1e6) {

  if (!"Grouped_Samples" %in% names(subgroup_object) ||
      !"Subgroup_Sizes" %in% names(subgroup_object) ||
      !"Allocate_Var" %in% names(subgroup_object)) {
    stop("Invalid subgroup object passed.")
  }

  allocs = find_possible_block_allocations( unlist(subgroup_object$Subgroup_Sizes),
                                            table(subgroup_object$Allocate_Var),
                                            fullTree = fullTree,
                                            maxCalls = maxCalls)

  allocs
}


mk_subgroup_shuffle_function = function(subgroup_object, subgroup_allocations,
                                        keep_separate_vars=c()) {

  force (subgroup_object)
  force(subgroup_allocations)
  force(keep_separate_vars)

  # Custom implementation of sample which deals with vectors of length 1 properly
  my_sample = function(x) {
    if (length(x) == 1) return(x)
    base::sample(x)
  }

  if (length(keep_separate_vars)>0) {
    sep_vars = intersect(keep_separate_vars, colnames(subgroup_object$Grouped_Samples))
    if (length(sep_vars)==0) {
      warning("No overlap between 'keep_separate_vars' and columns in sample table --> ignored.")
      sep_vars=NULL
    } else if (length(sep_vars)<length(keep_separate_vars)) {
      warning("Ignoring 'keep_separate_vars' not found in sample table: ",
              paste0(setdiff(keep_separate_vars, sep_vars), collapse=", "))
    }
  } else {
    sep_vars=NULL
  }

  grp_levels = levels(subgroup_object$Allocate_Var)

  # Explicitly spell out the different possible subgroup allocations on a sample level
  alloc_vectors = purrr::map(subgroup_allocations, rep.int, times=unlist(subgroup_object$Subgroup_Sizes))

  # Helper vector for the group level structure in the sample space
  group_vec = rep.int( seq_along(subgroup_object$Subgroup_Sizes),
                       purrr::map_dbl(subgroup_object$Subgroup_Sizes, sum) )

  # Helper vector for the subgroup level structure;
  # needed to preserve subgrouping info since same alloc variable could be used in different subgroups.
  # The idea is to keep group structure and allocation vectors fixed and permute the vector
  # with the indices that map each 'allocation variable' level to an actual sample, in order to
  # permute within each group and across the subgroups which are just there to satisfy size constraints.
  subgroup_vec = paste( group_vec,
                        rep.int(  unlist(purrr::map(subgroup_object$Subgroup_Sizes, seq_along)),
                                  unlist(subgroup_object$Subgroup_Sizes) ),
                        sep="_")

  # Index vector to assign group allocation to sample dataframe in its original order.
  # Split up according to group structure to allow easy permutation on the subgroup level
  sample_vec = order(dplyr::group_indices(subgroup_object$Grouped_Samples)) %>%
    split(f=group_vec)

  # Static memory for the actual returned allocation
  shuffle_index = integer(length(subgroup_vec))
  alloc_var_assigned = integer(length(subgroup_vec))
  subgroup_labels= character(length(subgroup_vec))

  idx=0 # index of currently selected allocation

  sep_fails_tolerance = 0 # number of violations allowed for the keep_separate_variable constraint

  # Function to return one shuffle proposal for the sample list in its original order
  # Values refer to the levels of the allocation variable
  # All constraints are guaranteed to be satisfied

  function(onlyShuffleIndex = TRUE, ...) {
    idx<<-idx+1
    if (idx>length(alloc_vectors)) {
      idx<<-1
    }
    # Current sample permutation vector, reshuffled
    rand_index = purrr::map(sample_vec, my_sample) %>% unlist(use.names=F)

    # Check if keep_separate_vars constraints are fulfilled within the radomized subgroups
    if (!is.null(sep_vars)) {
      fails = 0
      repeat {
        dups = purrr::map_int(sep_vars, ~ sum(duplicated(cbind(subgroup_object$Grouped_Samples[rand_index, .x], subgroup_vec))) )
        if (max(dups)<=sep_fails_tolerance) { break }
        fails = fails+1
        if (fails>1000) {
          fails=0
          sep_fails_tolerance <<- sep_fails_tolerance+1
          message("No permutations fulfilling the 'keep_separate' constraints in 1000 iters!\n",
                  "Increasing number of tolerated violations to ",sep_fails_tolerance)
        }
        rand_index = purrr::map(sample_vec, my_sample) %>% unlist(use.names=F)
      }
    }

    # Fill the values for a concrete sample permutation that justifies the constraints.
    alloc_var_assigned[rand_index] = alloc_vectors[[idx]] # the allocated variable
    subgroup_labels[rand_index] = subgroup_vec # the subgroup information
    shuffle_index = order(alloc_var_assigned, subgroup_labels) # indices to bring orig. sample list into shuffled order

    if (onlyShuffleIndex) {
      return(list(src = seq_along(shuffle_index),
                  dst = shuffle_index))
    }

    list(shuffle_index = shuffle_index, alloc_var_index=alloc_var_assigned,
         alloc_var_level=grp_levels[alloc_var_assigned], subgroup=subgroup_labels )
  }

}


shuffle_grouped_data = function(samples, allocate_var,
                                keep_together_vars=c(),
                                keep_separate_vars=c(),
                                n_min=NA, n_max=NA, n_ideal=NA,
                                prefer_big_groups=FALSE, strict=T,
                                fullTree = FALSE, maxCalls = 1e6) {

  sg = form_homogeneous_subgroups(samples = samples, allocate_var=allocate_var,
                                  keep_together_vars=keep_together_vars,
                                  n_min=n_min , n_max=n_max, n_ideal=n_ideal,
                                  prefer_big_groups=prefer_big_groups, strict=strict)

  sa = compile_possible_subgroup_allocation(sg, fullTree = fullTree, maxCalls = maxCalls)

  mk_subgroup_shuffle_function(sg, sa, keep_separate_vars = keep_separate_vars)

}
