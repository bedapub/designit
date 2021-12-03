

#' Internal helper function to set up an (n m) x (n m) pairwise distance matrix for a plate with n rows and m columns
#'
#' @param plate_x Dimension of plate in x direction (i.e number of columns)
#' @param plate_y Dimension of plate in y direction (i.e number of rows)
#' @param dist Distance function as understood by \code{stats::dist()}
#' @param p p parameter, used only if distance metric is 'minkowski'. Special cases: p=1 - Manhattan distance; p=2 - Euclidean distance
#'
#' @return The  matrix with pairwise distances between any wells on the plate
#' @keywords internal
mk_dist_matrix <- function(plate_x = 12, plate_y = 8, dist = "minkowski", p = 2) {
  # Helper function: Sets up a euclidean or alternative distance matrix (supported by stats::dist) for a generic x*y plate
  matrix(c(rep(1:plate_y, plate_x), rep(1:plate_x, each = plate_y)), ncol = 2) %>%
    stats::dist(method = dist, p = p) %>%
    as.matrix()
}

#' Create a list of scoring functions (one per plate) that quantify the spatially homogeneous distribution of conditions across the plate
#'
#' @param batch_container Batch container (bc) with all columns that denote plate related information
#' @param plate Name of the bc column that holds the plate identifier (may be missing or NULL in case just one plate is used)
#' @param row Name of the bc column that holds the plate row number (integer values starting at 1)
#' @param column Name of the bc column that holds the plate column number (integer values starting at 1)
#' @param group Name of the bc column that denotes a group/condition that should be distributed on the plate
#'
#' @return List of scoring functions, one per plate, that calculate a real valued measure for the quality of the group distribution (the lower the better)
#' @export
mk_plate_scoring_functions <- function(batch_container, plate = NULL, row, column, group) {
  MAX_PLATE_DIM <- 200

  # A function factory returning one specific scoring function
  # This may be called in any optimizer

  assertthat::assert_that(!is.null(batch_container), batch_container$has_samples,
    msg = "Batch container must have samples"
  )

  samp <- batch_container$get_samples(assignment = T, include_id = T, remove_empty_locations = F, as_tibble = T)

  assertthat::assert_that(row %in% colnames(samp),
    column %in% colnames(samp), group %in% colnames(samp),
    msg = "Batch container must contain all variables denoting row, column (of the plate) and a group"
  )

  assertthat::assert_that(is.null(plate) || plate %in% colnames(samp),
    msg = "Plate variable must be in batch container if more than one plate is used"
  )

  plate_scoring_func <- function(samples, plate, plate_name, row, column, group) {

    # Have to distinguish between empty positions on plate and NA-levels of experimental factors which may happen for real samples, too
    nonempty_pos <- !is.na(samples[[".sample_id"]])

    assertthat::assert_that(!all(is.na(samples[[group]][nonempty_pos])),
      msg = stringr::str_c("Group variable for plate ", plate_name, " must not be all NAs")
    )

    # Have to remember ALL levels of the Group factor since sample exchanges may happen across plates,
    # yielding different groups on the plate each time.
    # This includes NA as a dedicated level as well (note that <NA> will be always the last level in the level list!)
    group_fac <- factor(as.character(samples[[group]][nonempty_pos]), exclude = NULL)

    n_group <- nlevels(group_fac)
    group_lev_table <- stats::setNames(1:n_group, nm = levels(group_fac))

    if (!is.null(plate)) { # But now focus on the relevant plate only for remaining checks
      samples <- dplyr::filter(samples, !!as.symbol(plate) == plate_name)
    }

    assertthat::assert_that(!any(is.na(samples[[row]])), !any(is.na(samples[[column]])),
      msg = stringr::str_c("Row and column coordinates for plate ", plate_name, " must not include NAs")
    )
    assertthat::assert_that(is.integer(samples[[row]]), is.integer(samples[[column]]),
      msg = stringr::str_c("Row and column coordinates for plate ", plate_name, " must be integer values")
    )
    assertthat::assert_that(all(samples[[row]] > 0), all(samples[[row]] <= MAX_PLATE_DIM),
      msg = stringr::str_c("Row coordinates for plate ", plate_name, " must be within range 1...", MAX_PLATE_DIM)
    )
    assertthat::assert_that(all(samples[[column]] > 0), all(samples[[column]] <= MAX_PLATE_DIM),
      msg = stringr::str_c("Column coordinates for plate ", plate_name, " must be within range 1...", MAX_PLATE_DIM)
    )

    plate_x <- max(samples[[row]])
    plate_y <- max(samples[[column]])
    plate_pos <- plate_y * (samples[[row]] - 1) + samples[[column]]
    assertthat::assert_that(!any(duplicated(plate_pos)),
      msg = stringr::str_c("Plate coordinates must be unique for plate ", plate_name)
    )


    distance_matrix <- mk_dist_matrix(plate_x = plate_x, plate_y = plate_y)
    trial_template <- double(plate_x * plate_y) # filled with 0 initially (=no sample at any position)

    # Create and return the actual scoring function for a specific sample permutation

    function(bc) {
      samples <- bc$get_samples(assignment = T, include_id = T, remove_empty_locations = F, as_tibble = T)

      # Set up trial matrix from sample list
      trial <- trial_template

      # Filter out correct plate (if necessary) and non-empty plate locations
      if (is.null(plate)) {
        sel <- !is.na(samples[[".sample_id"]])
      } else {
        sel <- samples[[plate]] == plate_name & !is.na(samples[[".sample_id"]])
      }

      trial[plate_y * (samples[[row]][sel] - 1) + samples[[column]][sel]] <- group_lev_table[as.character(samples[[group]][sel])]

      trial[is.na(trial)] <- n_group # Cope for true NA levels in variables as indexing with NA yielded NAs at this step

      group_freq <- rle(sort(trial))
      loopover <- which(group_freq[["lengths"]] > 1) # elegantly skip groups which deliver an Inf score
      score <- 0

      for (i in loopover) {
        group_trial <- as.double(trial == group_freq[["values"]][i])
        # Omitting the explicit t() operation for the first argument in matrix multiplication saves 10% of CPU time.... :-|
        score <- score + ((group_freq[["lengths"]][i]^-2.6) * (group_trial %*% (distance_matrix %*% group_trial)))^-2
        # Better solution --> larger pairwise distances of samples of same group --> smaller overall score
      }

      score
    }
  }

  if (is.null(plate) || dplyr::n_distinct(dplyr::pull(samp, plate)) == 1) { # can skip differentiation of plates since there's only one
    score_funcs <- list("Plate" = plate_scoring_func(samp, plate = NULL, plate_name = "", row = row, column = column, group = group))
  } else {
    samp <- dplyr::group_by(samp, dplyr::across(dplyr::all_of(plate)))
    plate_names <- dplyr::pull(dplyr::group_keys(samp), 1)

    score_funcs <- purrr::map(plate_names, ~ plate_scoring_func(samp, plate = plate, plate_name = .x, row = row, column = column, group = group)) %>%
      stats::setNames(nm = paste("Plate", plate_names))
  }

  score_funcs
}