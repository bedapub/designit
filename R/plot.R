#' Plot batch design
#'
#' @param .tbl a [`tibble`][tibble::tibble()] (or `data.frame`).
#' @param ... columns to select for `x` axis (see [dplyr::select()]).
#' @param .color the variable used for coloring
#' @param .alpha the variable used for transparency
#'
#' @return The ggplot object.
#'
#' @noRd
plot_design <- function(.tbl, ..., .color, .alpha = NULL) {
  # generate vars
  vars <- rlang::enquos(...)
  combinations <- .tbl |>
    dplyr::mutate(combinations = interaction(!!!vars, lex.order = T)) |>
    dplyr::pull(combinations)
  g <- ggplot2::ggplot(.tbl) +
    ggplot2::aes(
      x = combinations,
      fill = {{ .color }}
    ) +
    ggplot2::geom_histogram(stat = "count") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  if (!rlang::quo_is_null(rlang::enquo(.alpha))) {
    alpha_levels <- .tbl |>
      dplyr::pull({{ .alpha }}) |>
      unique() |>
      length()
    alpha_range <- c(1 / min(5, alpha_levels), 1)
    g <- g +
      ggplot2::aes(alpha = {{ .alpha }}) +
      ggplot2::scale_alpha_ordinal(range = alpha_range)
  }
  return(g)
}


#' Plot plate layouts
#'
#' @param .tbl a [`tibble`][tibble::tibble()] (or `data.frame`) with the samples assigned to locations.
#' Alternatively a [BatchContainter][designit::BatchContainer()] with samples can be supplied here.
#' @param plate optional dimension variable used for the plate ids
#' @param row the dimension variable used for the row ids
#' @param column the dimension variable used for the column ids
#' @param .color the continuous or discrete variable to color by
#' @param .alpha a continuous variable encoding transparency
#' @param .pattern a discrete variable encoding tile pattern (needs ggpattern)
#' @param title string for the plot title
#' @param add_excluded flag to add excluded wells (in bc$exclude) to the plot.
#' A BatchContainer must be provided for this.
#' @param rename_empty whether NA entries in sample table should be renamed to 'empty`.
#'
#' @return the ggplot object
#' @export
#' @author siebourj
#'
#' @examples
#' nPlate <- 3
#' nColumn <- 4
#' nRow <- 6
#'
#' treatments <- c("CTRL", "TRT1", "TRT2")
#' timepoints <- c(1, 2, 3)
#'
#'
#' bc <- BatchContainer$new(
#'   dimensions = list(
#'     plate = nPlate,
#'     column = list(values = letters[1:nColumn]),
#'     row = nRow
#'   )
#' )
#'
#' sample_sheet <- tibble::tibble(
#'   sampleID = 1:(nPlate * nColumn * nRow),
#'   Treatment = rep(treatments, each = floor(nPlate * nColumn * nRow) / length(treatments)),
#'   Timepoint = rep(timepoints, floor(nPlate * nColumn * nRow) / length(treatments))
#' )
#'
#' # assign samples from the sample sheet
#' bc <- assign_random(bc, samples = sample_sheet)
#'
#' plot_plate(bc$get_samples(),
#'   plate = plate, column = column, row = row,
#'   .color = Treatment, .alpha = Timepoint
#' )
#'
#' \donttest{
#' plot_plate(bc$get_samples(),
#'   plate = plate, column = column, row = row,
#'   .color = Treatment, .pattern = Timepoint
#' )
#' }
plot_plate <- function(.tbl, plate = plate, row = row, column = column,
                       .color, .alpha = NULL, .pattern = NULL,
                       title = paste("Layout by", rlang::as_name(rlang::enquo(plate))),
                       add_excluded = FALSE,
                       rename_empty = FALSE) {
  # prevent undefined variable error
  Pattern <- NULL

  if (add_excluded) {
    assertthat::assert_that(R6::is.R6(.tbl) && inherits(.tbl, "BatchContainer"))
    excluded <- .tbl$exclude
  }

  if (R6::is.R6(.tbl) && inherits(.tbl, "BatchContainer")) {
    .tbl <- .tbl$get_samples()
  } else {
    assertthat::assert_that(is.data.frame(.tbl))
  }

  add_pattern <- FALSE
  # check parameters
  assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(row))))
  assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(column))))
  assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(.color))))
  if (!rlang::quo_is_null(rlang::enquo(.alpha))) {
    assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(.alpha))))
  }
  if (!rlang::quo_is_null(rlang::enquo(.pattern))) {
    add_pattern <- requireNamespace("ggpattern", quietly = TRUE)
    if (!add_pattern) {
      warning("Please install ggpattern to use patterns in the plot")
    } else {
      assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(.pattern))))
      .tbl <- .tbl |>
        dplyr::mutate(Pattern = as.factor({{ .pattern }}))
    }
  }
  # If there is no plate,
  if (rlang::quo_is_null(rlang::enquo(plate)) ||
    !assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(plate)))) {
    # check if row + column is unique
    assertthat::assert_that(
      (.tbl |> dplyr::count({{ column }}, {{ row }}) |> nrow()) == nrow(.tbl),
      msg = "Non-unique row + column combination found. Please provide a plate variable."
    )
    # make a fake plate variable
    .tbl <- .tbl |>
      dplyr::mutate(plate = 1)
    plate <- rlang::sym("plate")
  }
  # change NA values for empty wells
  if (rename_empty) {
    .color_name <- rlang::as_name(rlang::enquo(.color))
    .tbl[is.na(.tbl[[.color_name]]), .color_name] <- "empty"
  }
  # add excluded wells
  if (add_excluded) {
    .tbl <- dplyr::bind_rows(.tbl, excluded)
  }

  .tbl <- .tbl |>
    dplyr::mutate(
      plate = as.factor({{ plate }}),
      column = as.factor({{ column }}),
      row = as.factor({{ row }})
    )

  # make plot
  g <- ggplot2::ggplot(.tbl) +
    ggplot2::aes(x = column, y = row) +
    ggplot2::facet_wrap(dplyr::vars(plate), strip.position = "bottom") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_discrete(limits = rev(levels(.tbl$row))) +
    ggplot2::scale_x_discrete(position = "top")

  # scale alpha
  .alpha <- rlang::enquo(.alpha)
  if (!rlang::quo_is_null(.alpha)) {
    alpha_var <- .tbl |>
      dplyr::pull(!!.alpha)
    alpha_levels <- unique(alpha_var)
    if (is.numeric(alpha_var) && length(alpha_levels > 7)) {
      alpha_range <- c(1 / min(5, length(alpha_levels)), 1)
      g <- g +
        ggplot2::aes(alpha = !!.alpha) +
        ggplot2::scale_alpha(range = alpha_range)
    } else {
      alpha_levels <- factor(alpha_levels)
      alpha_range <- scales::rescale(
        as.numeric(alpha_levels),
        c(1 / min(5, length(alpha_levels)), 1)
      )
      names(alpha_range) <- levels(alpha_levels)
      g <- g +
        ggplot2::aes(alpha = factor(!!.alpha)) +
        ggplot2::scale_alpha_manual(
          name = rlang::as_label(.alpha),
          values = alpha_range
        )
    }
  }

  # set labels as original variables
  g <- g + ggplot2::xlab(rlang::as_name(rlang::enquo(column))) +
    ggplot2::ylab(rlang::as_name(rlang::enquo(row))) +
    ggplot2::ggtitle(title)

  # make tiles
  if (add_pattern) {
    g <- g + ggpattern::geom_tile_pattern(
      ggplot2::aes(
        fill = {{ .color }},
        pattern = Pattern
      ),
      colour = "grey50"
    ) +
      # this is required, see https://github.com/coolbutuseless/ggpattern/issues/50
      ggpattern::scale_pattern_discrete()
  } else {
    g <- g + ggplot2::geom_tile(
      ggplot2::aes(fill = {{ .color }}),
      colour = "grey50"
    )
  }

  return(g)
}
