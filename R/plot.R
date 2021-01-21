#' Plot batch design
#'
#' @param .tbl a [`tibble`][tibble::tibble()] (or `data.frame`).
#' @param ... columns to select for `x` axis (see [dplyr::select()]).
#' @param .color the variable used for coloring
#' @param .alpha the variable used for transparency
#'
#' @export
#'
#' @examples
#'
plot_design <- function(.tbl, ..., .color, .alpha = NULL) {
  # generate vars
  vars <- rlang::enquos(...)
  combinations <- .tbl %>%
    dplyr::mutate(combinations = interaction(!!!vars, lex.order = T)) %>%
    dplyr::pull(combinations)
  g <- ggplot2::ggplot(.tbl) +
    ggplot2::aes(
      x = combinations,
      fill = {{ .color }},
      color = {{ .color }}
    ) +
    ggplot2::geom_histogram(stat = "count") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  if (!rlang::quo_is_null(rlang::enquo(.alpha))) {
    alpha_levels <- .tbl %>%
      dplyr::select({{ .alpha }}) %>%
      unique() %>%
      length()
    alpha_range <- c(1 / min(5, alpha_levels), 1)
    g <- g +
      ggplot2::aes(alpha = {{ .alpha }}) +
      ggplot2::scale_alpha_ordinal(range = alpha_range)
  }
  print(g)
}


#' Plot plate layouts
#'
#' @param .tbl  a [`tibble`][tibble::tibble()] (or `data.frame`) with the samples assigned to locations
#' @param Plate the dimension variable used for the plate ids
#' @param Row the dimension variable used for the row ids
#' @param Column the dimension variable used for the column ids
#' @param .color the continuous or discrete variable to color by
#' @param .alpha a continuous variable encoding transparency
#' @param .pattern a discrete variable encoding tile pattern (needs ggpattern)
#'
#' @return
#' @export
#' @author siebourj
#'
#' @examples
#'
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
#' assign_random(bc, samples = sample_sheet)
#'
#' bc$get_samples()
#'
#' plot_plate(bc$get_samples(),
#'   Plate = plate, Column = column, Row = row,
#'   .color = Treatment, .alpha = Timepoint
#' )
#'
#' plot_plate(bc$get_samples(),
#'   Plate = plate, Column = column, Row = row,
#'   .color = Treatment, .pattern = Timepoint
#' )
plot_plate <- function(.tbl, Plate = Plate, Row = Row, Column = Column,
                       .color, .alpha = NULL, .pattern = NULL) {
  add_pattern <- FALSE
  # check dimensions
  assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(Plate))))
  assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(Row))))
  assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(Column))))
  assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(.color))))
  if (!rlang::quo_is_null(rlang::enquo(.alpha))) {
    assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(.alpha))))
  }
  if (!rlang::quo_is_null(rlang::enquo(.pattern))) {
    assertthat::assert_that(assertthat::has_name(.tbl, rlang::as_name(rlang::enquo(.pattern))))
    assertthat::assert_that(requireNamespace("ggpattern", quietly = TRUE),
      msg = "Please install ggpattern to use patterns in the plot"
    )
    add_pattern <- TRUE

    .tbl <- .tbl %>%
      dplyr::mutate(Pattern = factor({{ .pattern }}))
  }

  .tbl <- .tbl %>%
    dplyr::mutate(
      Plate = factor({{ Plate }}),
      Column = factor({{ Column }}),
      Row = factor({{ Row }})
    )

  # make plot
  g <- ggplot2::ggplot(.tbl) +
    ggplot2::aes(x = Column, y = Row) +
    ggplot2::facet_wrap(dplyr::vars(Plate), strip.position = "bottom") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_discrete(limits = rev(unique(.tbl %>% dplyr::pull(Row)))) +
    ggplot2::scale_x_discrete(position = "top")

  # scale alpha
  if (!rlang::quo_is_null(rlang::enquo(.alpha))) {
    alpha_levels <- .tbl %>%
      dplyr::pull({{ .alpha }}) %>%
      unique() %>%
      length()
    alpha_range <- c(1 / min(5, alpha_levels), 1)
    g <- g +
      ggplot2::aes(alpha = {{ .alpha }}) +
      ggplot2::scale_alpha(range = alpha_range)
    # ggplot2::scale_alpha_ordinal(range = alpha_range) # visually not a good idea
  }

  # set labels as original variables
  g <- g + ggplot2::xlab(rlang::as_name(rlang::enquo(Column))) +
    ggplot2::ylab(rlang::as_name(rlang::enquo(Row))) +
    ggplot2::ggtitle(paste("Layout by", rlang::as_name(rlang::enquo(Plate))))

  # make tiles
  if (add_pattern) {
    g <- g + ggpattern::geom_tile_pattern(
      ggplot2::aes(
        fill = {{ .color }},
        pattern = Pattern
      ),
      colour = "grey50"
    )
  } else {
    g <- g + ggplot2::geom_tile(ggplot2::aes(fill = {{ .color }}),
      colour = "grey50"
    )
  }

  return(g)
}
