#' Plot batch design
#'
#' @param .tbl a [`tibble`][tibble::tibble()] (or `data.frame`).
#' @param ... columns to select for `x` axis (see [dplyr::select()]).
#' @param .color the variable used for coloring
#' @param .alpha the variable used for transparency
#'
#' @export
plot_design <- function(.tbl, ..., .color, .alpha = NULL) {
  # generate vars
  vars <- rlang::enquos(...)
  combinations <- .tbl %>%
    dplyr::mutate(combinations = interaction(!!!vars, lex.order = T)) %>%
    dplyr::pull(combinations)
  g <- ggplot2::ggplot(.tbl) +
    ggplot2::aes(x = combinations,
                 fill = {{.color}},
                 color = {{.color}}
                 ) +
    ggplot2::geom_histogram(stat = "count") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  if (!rlang::quo_is_null(rlang::enquo(.alpha))) {
    alpha_levels <- .tbl %>% dplyr::select({{.alpha}}) %>% unique() %>% length()
    alpha_range <- c(1 / min(5, alpha_levels), 1)
    g <- g +
      ggplot2::aes(alpha = {{.alpha}}) +
      ggplot2::scale_alpha_ordinal(range = alpha_range)
  }
  print(g)
}
