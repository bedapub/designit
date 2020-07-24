#' Generate block design
#'
#' @param .tbl a [`tibble`][tibble::tibble()] (or `data.frame`).
#' @param k number of batches (blocks), a single integer, a vector of batch lengths, or a vector of batch IDs.
#' @param ... columns to select (see [dplyr::select()]).
#' @param .name name of the batch (block) column.
#' @param .prefix prefix of batch values ("B" for B1, B2, etc.).
#' @param .control list of arguments for optFederov and optBlock.
#'
#' @return sample list with the batch
#' @export
#'
#' @examples
#'
generate_blocks <- function(.tbl,
                            k,
                            ...,
                            .name = "Batch",
                            .prefix = "B",
                            .control = list()) {
  variables <- rlang::enquos(...)
  .tbl <- .tbl %>%
    dplyr::mutate(.id_for_samples = 1:dplyr::n())
  x <- .tbl %>%
    dplyr::select(..., .id_for_samples)
  mt <- generate_terms(x)
  # mm <- stats::model.matrix(mt, data = x)
  # mm <- mm[, estimable(mm)]
  n <- nrow(x)
  des <- AlgDesign::optFederov(frml = mt,
                               data = x,
                               nTrials = n,
                               nRepeats = 20,
                               args = TRUE)
  if (length(k) == 1) {
    r <- ceiling(n / k) # group sizes
    blocksizes <- rep(r, k)
    if (k * r != n) blocksizes[length(blocksizes)] <- n %% r # shorten last batch
  } else if (length(k) < n) {
    if (rep(seq_along(k), times = k) != n) stop("block sizes are not matching the sample size (rows of data).", call. = FALSE)
    blocksizes <- k
  } else if (length(k) == n) {
    blocksizes <- as.vector(table(k)) # convert a full vector of batch IDs to group sizes
  } else {
    stop("wrong number of batches (`k`).", call. = FALSE)
  }
  des_blk <- AlgDesign::optBlock(frml = mt,
                                 withinData = des$design,
                                 blocksizes = blocksizes,
                                 nRepeats = 100,
                                 args = TRUE)
  des_blk_lst <- lapply(des_blk$Blocks, tibble::rownames_to_column, var = "OrderWithin")
  des_blk_tbl <- dplyr::bind_rows(des_blk_lst, .id = "Block") %>%
    dplyr::mutate(Block = Block %>%
                    stringr::str_remove("^B") %>%
                    stringr::str_pad(
                      max(0, max(stringr::str_length(Block)) - 1),
                      pad = "0")) %>%
    dplyr::group_by(Block) %>%
    dplyr::mutate(OrderWithin = sample(OrderWithin)) %>%
    dplyr::ungroup()
  res <- .tbl %>%
    dplyr::left_join(des_blk_tbl,
                     by = c(
                       sapply(variables, rlang::quo_name),
                       ".id_for_samples"
                       )
                     ) %>%
    dplyr::mutate(Block = stringr::str_c(.prefix,
                                         stringr::str_remove(Block, "B")),
                  .id_for_samples = NULL) %>%
    dplyr::rename({{.name}} := Block)
  des$design <- NULL
  des$args.data <- NULL
  des_blk$design <- NULL
  des_blk$withinData <- NULL
  des_blk$Blocks <- NULL
  des_blk$args.data <- NULL
  attributes(res) <- append(attributes(res),
                            list(args.federov = des,
                                 args.block = des_blk)
                            )
  res
}

#' TODO:
#'   * add SampleID column, match based on that and the common columns
#'     left_join(x, y, by = c(rlang::quo_name(SampleID), sapply(variables, rlang::quo_name)))
#'   * check if other package can generate a sparse design
#'
#'
#' Meeting minutes:
#' * weighting of variables (in case of too few samples)
#'
