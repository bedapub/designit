#' Moran's I spatial structure score
#'
#' Compute a Moran's I–based score for a single 2D plate layout or 1D sequence.
#' The score penalizes spatial structure; **smaller values indicate better
#' randomization**. Both clustering (positive Moran's I) and over-alternation
#' (negative Moran's I) are penalized equally via \eqn{|I|}.
#'
#' This function is intended for **design scoring**, not hypothesis testing.
#' P-values are not computed or used.
#'
#' @param bc A \code{BatchContainer} (from \pkg{designit}) or a
#'   \code{data.frame} containing sample assignments and locations.
#' @param feature_var Character scalar. Column name of the categorical feature
#'   to score (e.g. treatment, condition).
#' @param location_vars Character vector of length 1 or 2 specifying the
#'   location variables:
#'   \itemize{
#'     \item length 1: 1D sequence (e.g. time, position)
#'     \item length 2: 2D grid or plate (row, column)
#'   }
#' @param neighbor Character. Neighborhood definition:
#'   \code{"rook"} or \code{"queen"} for 2D grids;
#'   \code{"chain"} or \code{"circle"} for 1D sequences.
#' @param aggregate Character. How to aggregate across factor levels:
#'   \code{"max_abs"} (default) or \code{"mean_abs"}.
#' @param style Character. Weight style passed to \code{spdep::nb2listw()},
#'   default \code{"W"} (row-standardized).
#' @param zero_policy Logical. Passed to \code{spdep::nb2listw()} to allow
#'   locations with no neighbors.
#'
#' @return A single non-negative numeric score. Smaller values indicate less
#'   spatial structure.
#'
#' @details
#' For categorical features, Moran's I is computed using a one-vs-rest
#' indicator for each level. The final score aggregates the absolute Moran's I
#' values across levels.
#'
#' Missing feature values or missing locations are ignored.
#'
#' @examples
#' \dontrun{
#' moran_score(bc, "Treatment", c("row", "col"))
#' moran_score(df, "Condition", "position", neighbor = "chain")
#' }
#'
#' @seealso \code{\link[designit]{osat_score}}
#' @importFrom stats var
#' @export
moran_score <- function(
    bc,
    feature_var,
    location_vars,
    neighbor = c("rook", "queen", "chain", "circle"),
    aggregate = c("max_abs", "mean_abs"),
    style = "W",
    zero_policy = TRUE
) {
  neighbor  <- match.arg(neighbor)
  aggregate <- match.arg(aggregate)

  if (!requireNamespace("spdep", quietly = TRUE)) {
    stop("Package 'spdep' is required for moran_score().")
  }

  samples <- if (inherits(bc, "BatchContainer")) {
    bc$get_samples(remove_empty_locations = TRUE)
  } else {
    bc
  }

  if (!all(c(feature_var, location_vars) %in% names(samples))) {
    stop("feature_var and location_vars must exist in the data.")
  }

  keep <- !is.na(samples[[feature_var]])
  for (v in location_vars) keep <- keep & !is.na(samples[[v]])
  samples <- samples[keep, , drop = FALSE]

  if (nrow(samples) < 3) return(0)

  build_listw <- function(samples) {
    if (length(location_vars) == 1) {
      pos <- as.integer(samples[[location_vars]])
      ord <- order(pos)
      n <- length(pos)

      nb <- lapply(seq_len(n), function(i) {
        neigh <- integer(0)
        if (i > 1) neigh <- c(neigh, i - 1)
        if (i < n) neigh <- c(neigh, i + 1)
        if (neighbor == "circle" && n > 2) {
          if (i == 1) neigh <- c(neigh, n)
          if (i == n) neigh <- c(neigh, 1)
        }
        neigh
      })

      list(
        listw = spdep::nb2listw(nb, style = style, zero.policy = zero_policy),
        ord = ord
      )
    } else if (length(location_vars) == 2) {
      r <- as.integer(samples[[location_vars[1]]])
      c <- as.integer(samples[[location_vars[2]]])

      n_row <- max(r)
      n_col <- max(c)
      cell <- (c - 1L) * n_row + r

      ord <- order(cell)
      keep_set <- sort(unique(cell))
      map <- setNames(seq_along(keep_set), keep_set)

      nb_full <- spdep::cell2nb(
        n_row, n_col,
        type = if (neighbor == "queen") "queen" else "rook"
      )

      nb_kept <- lapply(keep_set, function(k) {
        unname(map[as.character(nb_full[[k]][nb_full[[k]] %in% keep_set])])
      })

      list(
        listw = spdep::nb2listw(nb_kept, style = style, zero.policy = zero_policy),
        ord = ord,
        idx = unname(map[as.character(cell[ord])])
      )
    } else {
      stop("location_vars must have length 1 or 2.")
    }
  }

  built <- build_listw(samples)
  lw <- built$listw

  g <- samples[[feature_var]][built$ord]
  if (!is.null(built$idx)) g <- g[order(built$idx)]

  g <- droplevels(as.factor(g))
  lvls <- levels(g)
  if (length(lvls) < 2) return(0)

  S0 <- spdep::Szero(lw)
  I_abs <- vapply(lvls, function(lv) {
    x <- as.numeric(g == lv)
    if (var(x) == 0) return(0)
    I <- spdep::moran(x, lw, length(x), S0)$I
    # pmax(I, 0) if only penalize clustering
    abs(ifelse(is.finite(I), I, 0)) # Why replace inf with 0?
  }, numeric(1))

  if (aggregate == "max_abs") max(I_abs) else mean(I_abs)
}


#' Moran's I score generator for designit
#'
#' Create a cached Moran's I scoring function suitable for iterative
#' optimization with \pkg{designit}. The neighborhood structure is computed
#' once and reused for speed.
#'
#' @param feature_var Character scalar. Column name of the categorical feature.
#' @param location_vars Character vector of length 1 or 2 specifying the layout.
#' @param neighbor Character. See \code{\link{moran_score}}.
#' @param aggregate Character. See \code{\link{moran_score}}.
#' @param style Character. Weight style for spatial weights.
#' @param zero_policy Logical. Passed to \code{spdep::nb2listw()}.
#'
#' @return A function taking a \code{BatchContainer} and returning a numeric
#'   Moran score.
#'
#' @seealso \code{\link[designit]{osat_score_generator}}
#' @export
moran_score_generator <- function(
    feature_var,
    location_vars,
    neighbor = c("rook", "queen", "chain", "circle"),
    aggregate = c("max_abs", "mean_abs"),
    style = "W",
    zero_policy = TRUE
) {
  neighbor  <- match.arg(neighbor)
  aggregate <- match.arg(aggregate)

  cache <- new.env(parent = emptyenv())
  cache$ready <- FALSE

  function(bc) {
    if (!cache$ready) {
      samples <- bc$get_samples(remove_empty_locations = TRUE)
      keep <- TRUE
      for (v in location_vars) keep <- keep & !is.na(samples[[v]])
      samples <- samples[keep, , drop = FALSE]

      tmp_bc <- samples
      cache$builder <- moran_score
      cache$built <- moran_score(
        tmp_bc, feature_var, location_vars,
        neighbor = neighbor,
        aggregate = aggregate,
        style = style,
        zero_policy = zero_policy
      )
      cache$ready <- TRUE
    }

    moran_score(
      bc, feature_var, location_vars,
      neighbor = neighbor,
      aggregate = aggregate,
      style = style,
      zero_policy = zero_policy
    )
  }
}
