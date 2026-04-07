#' Create a Shuffle-Function that Places Each Group into Exactly One Batch
#'
#' This function returns another function (a "shuffle-proposal function") that
#' can be used in `optimize_design()`. The returned function will assign each group
#' (e.g., each "Litter") so that it stays on a single batch (e.g., a specific "plate").
#'
#' Internally, it sorts groups by descending size and places them in batches
#' that have enough *unoccupied* capacity. If a group cannot fit, it retries a random
#' assignment from scratch. If it fails after `max_retries`, it stops with an error.
#'
#' @param batch_var Column in the container locations that defines the batch
#'   (e.g. `"plate"`).
#' @param group_id Column in the sample data that defines the group
#'   (e.g. `"Litter"`).
#' @param max_retries How many times to retry if a group doesn't fit anywhere.
#' @param quiet Whether to suppress progress messages about retries.
#'
#' @return A function `(batch_container, iteration) -> integer vector`
#'   suitable as `shuffle_proposal_func` in `optimize_design()`.
#'   The integer vector (length = # of locations) is the new `.sample_id` assignment
#'   per location row.
#'
#' @examples
#' library(designit)
#'
#' data("invivo_study_samples")
#' bc <- BatchContainer$new(dimensions = c("plate" = 4, "column" = 5, "row" = 3)) |>
#' # we sort by Strain to ensure poor assignment
#"  assign_in_order(dplyr::arrange(invivo_study_samples, Strain))
#'
#' # Shuffle so each Litter remains on a single plate, up to 5 random tries
#' shuffle_func <- mk_group_batch_shuffler("plate", "Litter")
#'
#' set.seed(43)
#' bc_opt <- optimize_design(
#'   batch_container = bc,
#'   scoring = osat_score_generator("plate", "Strain"),
#'   shuffle_proposal_func = shuffle_func,
#'   max_iter = 50
#' )
#' @export
mk_group_batch_shuffler <- function(
    batch_var = "plate",
    group_id = "Litter",
    max_retries = 100L,
    quiet = FALSE) {
  ## gpt-generated
  # Fix these parameters in the returned function
  force(batch_var)
  force(group_id)
  force(max_retries)
  force(quiet)

  # The function that optimize_design() will call each iteration
  function(batch_container, iteration) {
    # 1) Get location info & find how many FREE slots each batch has
    loc_df <- batch_container$get_locations() |>
      dplyr::mutate(.location_id = dplyr::row_number())
    n_loc <- nrow(loc_df)
    stopifnot(batch_container$has_samples)

    if (!batch_var %in% colnames(loc_df)) {
      stop("'", batch_var, "' not found in the container locations.")
    }

    # For each location, see if it's free:
    #   assignment[i] is NA -> location i is free.
    current_assign <- batch_container$assignment
    capacity_df <- batch_container$get_locations() |>
      dplyr::group_by(.data[[batch_var]]) |>
      dplyr::summarize(
        capacity = dplyr::n(),
        .groups = "drop"
      )
    batch_levels <- capacity_df[[batch_var]]

    # 2) Gather sample data to see how many assigned samples are in each group
    samp_df <- batch_container$get_samples(
      include_id = TRUE,
      remove_empty_locations = TRUE,
      as_tibble = FALSE
    )

    if (!group_id %in% colnames(samp_df)) {
      stop("'", group_id, "' not found among the sample columns.")
    }

    # Only consider actually assigned samples (with .sample_id != NA)
    group_sizes <- samp_df |>
      dplyr::group_by(.data[[group_id]]) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data[["count"]]))

    # If no assigned samples, just return the existing assignment
    stopifnot(nrow(group_sizes) > 0L)

    # 3) A helper that tries one random assignment of group->batch
    #    returning a df with (group, batch) or NULL if fail
    try_assign_once <- function() {
      # local copy of free capacity
      free_cap <- capacity_df$capacity

      out <- vector("list", nrow(group_sizes))
      for (i in seq_len(nrow(group_sizes))) {
        g_size <- group_sizes[["count"]][i]
        # feasible batches?
        feasible_batches <- which(free_cap >= g_size)

        if (length(feasible_batches) == 0L) {
          return(NULL)
        }
        # randomly pick 1 feasible batch
        # note: sample(vec, 1) returns incorrect results when vector is a single
        # integer
        chosen_idx <- feasible_batches[sample.int(length(feasible_batches), 1L)]
        # reduce capacity
        free_cap[chosen_idx] <- free_cap[chosen_idx] - g_size

        out[[i]] <- data.frame(
          group = group_sizes[[group_id]][i],
          batch = batch_levels[chosen_idx],
          stringsAsFactors = FALSE
        )
      }
      dplyr::bind_rows(out)
    }

    # 4) Try up to max_retries. If we get a valid group->batch map, proceed
    group_assign <- NULL
    attempt <- 1L
    for (attempt in seq_len(max_retries)) {
      ga <- try_assign_once()
      if (!is.null(ga)) {
        group_assign <- ga
        break
      }
    }
    if (is.null(group_assign)) {
      warning("Could not place all groups into batches after ", max_retries, " tries.")
      return(NULL)
    } else if (!quiet && attempt > 1) {
      # message("Groups assigned successfully on attempt #", attempt)
    }

    # 5) Build new assignment vector. We set sample assignment
    #    according to the new group->batch mapping (in order).
    new_assign <- rep(NA_integer_, n_loc)

    # note: sample_assign tells us each sample's group => batch
    sample_assign <- samp_df |>
      dplyr::select(
        ".sample_id",
        group_id = dplyr::all_of(group_id)
      ) |>
      dplyr::left_join(group_assign, by = c("group_id" = "group"))

    # Split location rows by batch
    loc_split <- split(loc_df, loc_df[[batch_var]])
    # Also split the sample df by assigned batch
    sample_split <- split(sample_assign, sample_assign[["batch"]])

    # For each batch level, fill its free slots with the assigned samples
    # in a consistent order.
    for (b_lev in names(loc_split)) {
      sub_loc <- loc_split[[b_lev]]
      # which row indices in the original loc_df are these?
      idx_in_loc_df <- sub_loc[[".location_id"]]

      # The samples assigned to this b_lev (may be NULL if no group assigned)
      smp_sub <- sample_split[[b_lev]]
      if (!is.null(smp_sub)) {
        n_smp <- nrow(smp_sub)
        n_slot <- nrow(sub_loc)
        if (n_smp > n_slot) {
          # This shouldn't happen if capacity checks are correct.
          # We treat it as a "failed attempt" => return NULL
          stop("Internal error: more samples than slots in batch ", b_lev)
        }
        # fill them in order
        new_assign[idx_in_loc_df[seq_len(n_smp)]] <- smp_sub[[".sample_id"]]
      }
    }

    # If we made it this far, new_assign is good.
    # Return the new assignment vector to the optimizer.
    new_assign
  }
}
