# All functions used by the 'randomization procedure' from Guido and Juliane
# to be kept for reference and removed when the new workflow can cover this.


#' Gini index on a factor vector of counts using the Gini function
#' from the package ineq
#' @title Gini index on counts
#' @param m a factor vector
#' @return the value of the Gini index
#'
#' @author Juliane Siebourg-Polster
#'
#' @export
#'
countGini <- function(m) {
  lev <- nlevels(m)
  m <- table(m, useNA = "no")
  return(ineq::Gini(c(m, rep(0, (lev - length(m))))))
}


#' Mean differens on vector to score continuous variables
#' @title Mean difference
#' @param m a numeric vector
#' @return the mean difference
#'
#' @author Juliane Siebourg-Polster
#'
#' @export
#'
meanDiff <- function(m) {
  (max(m) - min(m)) / mean(m)
}


#' Kruskal-Wallis test statistic from the ranking of a factor vector
#' TODO: This is currently not used, it does not optimize for the correct layout
#'
#' @title  Kruskal Wallis on Layout dimension
#' @param m a factor vector
#' @return the Kruskal-Wallis rank sum statistic
#'
#' @author Juliane Siebourg-Polster
#'
kruskal <- function(m) {
  m <- stats::na.omit(m)
  x <- as.numeric(1:length(m))
  if (length(unique(m)) == 1) {
    kw <- as.double(length(levels(m))) # everything from the same group -> some large value
  } else {
    kw <- stats::kruskal.test(x, m)$statistic
  }
  return(kw)
}


#' Penalty score for number of neighboring pairs with the same level of a factor.
#' The number of such pairs is summed.
#' @title  Penalty for neighbors with same level
#' @param m a factor vector
#' @return the number of pairs
#'
#' @author Juliane Siebourg-Polster
#'
#' @export
#'
neighbors <- function(m) {
  x <- sum(m[1:(length(m) - 1)] == m[2:length(m)])
  return(x)
}


#' Summary score for an experimental layout based on the distribution of levels
#' of the factors to be balanced.
#' @title get Score
#' @param layout a data.frame with the factors to be balanced and their current positions
#' @param balance a vector with the names of experimental conditions to be balanced
#' @param sc_weights named vector of weights for the dimensions (default all 1)
#' @param bal_weights named vector of weights for the factors to be balanced (default all 1)
#' @param sc_groups list of dimension name groups to be used jointly for scoring
#' @param sc_tests list of tests to use for each scoring group
#'
#' @return the summarized penalty score
#'
#' @author Juliane Siebourg-Polster
#'
#' @export
#'
#'
getScore <- function(layout, balance, sc_groups, sc_tests,
                     bal_weights = rep(1, length(balance)),
                     sc_weights = rep(1, length(sc_groups))) {
  scores_dim <- c() # vector to store the score of each dimension group
  for (s in 1:length(sc_groups)) {
    tscore <- 0
    group <- sc_groups[[s]]
    tests <- sc_tests[[s]]
    for (ti in 1:length(tests)) {
      # make test function from given string
      testFun <- function(x) {
        do.call(tests[[ti]], list(x))
      }

      if (tests[[ti]] != "meanDiff") {
        # get the penalty for each dimension group
        # factor von balace variables
        penalty <- layout %>%
          dplyr::group_by(!!sym(group)) %>%
          dplyr::summarize_at(.vars = dplyr::vars(balance), testFun)
        score <- penalty %>%
          dplyr::summarize(score = sum((!!sym(balance))^2, na.rm = TRUE))
        score <- sum(score * bal_weights) # multiply score for each balance factor by its weight and sum
        tscore <- tscore + score
      } else {
        # loop over balance variables
        means <- layout %>%
          dplyr::summarize_at(.vars = dplyr::vars(balance), .funs = mean)
        # uselevel <- ifelse(length(groups)>1, group[2])
        diff <- means %>% dplyr::summarize(.vars = dplyr::vars(balance), meanDiff)
        score <- sum(diff * bal_weights) # multiply score for each balance factor by its weight and sum
        tscore <- tscore + score
      }
    }
    scores_dim <- c(scores_dim, tscore)
  }
  return(sum(scores_dim * sc_weights))
}


#' This function generates a randomized experimental layout based on given
#' experimental dimensions and factors to be balanced
#'
#' @title randomize experimental layout
#' @param design a data.frame with the sample ids, experimental conditions and
#' information about fixed samples (columns with 'Fix' prefix and then the dimension name)
#' @param report a string with the sample identifier
#' @param layout_dim a named vector with the experimental dimensions
#' @param balance a vector with the names of experimental conditions to be balanced
#' @param scoring_groups list of dimension name groups to be used jointly for scoring
#' @param scoring_tests list of dimension name groups to be used jointly for scoring
#' @param burnin a number of initial burnin runs (default=100)
#' @param annealprotocol a vector with the number of pairs to swap in each annealing step
#' @param scoring_weights named vector of weights for the dimensions (default all 1)
#' @param balance_weights named vector of weights for the factors to be balanced (default all 1)
#' @param distribute a starting distribution
#'
#' @return the value of the Gini index
#'
#' @author Juliane Siebourg-Polster
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # samples to use
#' samples <- data.frame(
#'   Group = c(1, 2, 3, 4, 5),
#'   Treatment = c(
#'     "vehicle", "RTR24", "RTR25",
#'     "RTR26", "RTR27"
#'   ),
#'   Dose = c(0, 25, 25, 25, 25),
#'   animals = c(2, 2, 2, 2, 2)
#' )
#'
#' # generate initial sample table (2 animals per group with 3 replicates)
#' samples <- dplyr::bind_rows(samples %>% dplyr::mutate(animals = 1), samples) %>%
#'   dplyr::rename(animal = animals)
#' samples <- dplyr::bind_rows(list(samples, samples, samples)) %>%
#'   dplyr::mutate(
#'     replicate = rep(1:3, each = 10),
#'     SampleID = paste0(Treatment, "_", animal, "_", replicate),
#'     AnimalID = paste0(Treatment, "_", animal)
#'   )
#'
#' # to be put on a 96 well plate
#' # (empty wells: first and last column plus two more wells)
#' empty <- 8 * 4 - nrow(samples) # n locations - n samples
#' emptydf <- data.frame(
#'   Treatment = "empty",
#'   FixColumn = 4, FixRow = 8 + 1 - (1:empty)
#' )
#'
#' # final sample table
#' design <- dplyr::full_join(samples, emptydf)
#'
#' # set parameters
#' layout_dim <- c(Row = 8, Column = 4)
#' scoring_groups <- list(c("Row"), c("Column"))
#' scoring_tests <- list("countGini", "countGini")
#' scoring_weights <- c(Row = 1, Column = 2)
#' balance <- c("AnimalID")
#' balance_weights <- c(1, 1)
#' names(balance_weights) <- balance
#' report <- "SampleID" # column with a unique ID
#' # annealprotocol <- rep(c(10,5,2,1), c(500,1000,2000,5000))
#' annealprotocol <- rep(c(10, 5, 2, 1), c(50, 100, 200, 300))
#'
#' # run randomization
#' result <- randomize(design, report, layout_dim, balance,
#'   scoring_groups = scoring_groups,
#'   scoring_tests = scoring_tests,
#'   burnin = 200, annealprotocol = annealprotocol,
#'   scoring_weights = scoring_weights,
#'   balance_weights = balance_weights,
#'   distribute = sample(1:(prod(layout_dim)))
#' )
#'
#' final_design <- result$design %>%
#'   dplyr::mutate(Column = Column + 1) # first column empty
#'
#' # plot
#' library(ggplot)
#' ggplot(
#'   final_design,
#'   aes(x = Column, y = Row, fill = Treatment, alpha = factor(animal))
#' ) +
#'   theme_minimal() +
#'   geom_tile() +
#'   scale_x_continuous(breaks = unique(final_design$Column)) +
#'   scale_y_reverse(breaks = rev(unique(final_design$Row))) +
#'   scale_alpha_manual(values = c(1, 0.5))
#'
#' kable(table(final_design$Treatment, final_design$Column),
#'   digits = 0,
#'   caption = "Treatment distribution across Columns."
#' )
#'
#' # optimization curve
#' plot(score ~ iteration,
#'   data = result$opti,
#'   log = "x", ylab = "penalty", type = "b"
#' )
#' }
#'
randomize <- function(design, report, layout_dim, balance,
                      # scorings
                      scoring_groups = as.list(names(layout_dim)),
                      scoring_tests = as.list(rep("countGini", length(layout_dim))),
                      # annealing params
                      burnin = 100, annealprotocol,
                      # weights
                      scoring_weights = rep(1, length(scoring_groups)),
                      balance_weights = rep(1, length(balance)),
                      # initial sample distribution
                      distribute = 1:(prod(layout_dim))) {

  # initialization
  nloc <- prod(layout_dim) # available locations
  nsamp <- nrow(design) # number of samples
  ndim <- length(layout_dim) # number of experiment dimensions
  grid <- expand.grid(sapply(layout_dim, function(x) {
    seq(1, x)
  }, simplify = F)) %>% # TODO: maybe lapply?

    as.data.frame()

  shuffle <- rep.int(0, nloc) # vectors to track fixed fixedPos
  fixed <- rep(FALSE, nloc)

  # make factors
  design <- design %>%
    dplyr::mutate_at(.vars = dplyr::vars(balance), factor)

  # Adding empty samples
  if (nloc < nsamp) {
    stop("too many samples")
  } else if (nloc > nsamp) {
    nempty <- nloc - nsamp
    message("\n Adding", nempty, "empty sample(s) to fill layout\n")
    design[(nsamp + 1):nloc, ] <- matrix(NA, ncol = ncol(design), nrow = nempty)
  }

  # get fixed position columns
  fixedPos <- design[, grep("Fix", colnames(design))]
  if (length(fixedPos) > 0) {
    # convert to numeric
    convert <- colnames(fixedPos)[sapply(fixedPos, is.character)]
    if (length(convert) > 0) {
      fixedPos[, convert] <- lapply(
        convert, function(col) {
          dim <- sub("Fix", "", col)
          fixedPos[, col] <- toupper(fixedPos[, col])
          fixedPos[, col] <- factor(fixedPos[, col], labels = 1:layout_dim[dim])
          as.numeric(fixedPos[, col])
        }
      )
    }
    # put columns in same order as layout_dim
    fixedPos <- fixedPos[, paste0("Fix", names(layout_dim))]

    # fixed position index vector
    if (ncol(fixedPos) == length(layout_dim)) {
      # fixed position indicator
      f <- apply((!is.na(fixedPos)), 1, all)
      # fixed vector index
      idxf <- apply(
        fixedPos[which(f), 1:ndim], 1,
        function(pos) {
          sum((pos - c(0, rep(1, ndim - 1))) *
            cumprod(c(1, layout_dim[1:(ndim - 1)])))
        }
      )
      fixed[idxf] <- TRUE
      shuffle[idxf] <- (1:nloc)[f]
      if (length(distribute) == length(f)) {
        distribute <- distribute[!f]
      }
    }
    # TODO: add something for reusable layout,
    # i.e. same fixed rows / cols per each plate
  }

  optiX <- optiY <- vector() # vectors to track optimization
  shuffle[!fixed] <- distribute # update shuffle vector

  bestdist <- distribute
  bestsol <- shuffle


  # sort balance factors by current layout and get penalty score
  mfac <- cbind(grid, design[shuffle, ])
  globalmin <- getScore(
    layout = mfac, balance = balance, sc_groups = scoring_groups,
    sc_tests = scoring_tests, bal_weights = balance_weights,
    sc_weights = scoring_weights
  )
  message("start optimization from minimum:", globalmin, "\n")


  # Start of optimization

  # 1. 'burnin' phase with complete reshuffling
  for (run in 1:burnin) {
    # new distribution + update shuffle vector
    distribute <- sample(distribute)
    shuffle[!fixed] <- distribute

    # scoring
    mfac <- cbind(grid, design[shuffle, ])
    penalty <- getScore(
      layout = mfac, balance = balance, sc_groups = scoring_groups,
      sc_tests = scoring_tests, bal_weights = balance_weights,
      sc_weights = scoring_weights
    )

    # update results
    if (penalty < globalmin) {
      globalmin <- penalty
      bestsol <- shuffle
      bestdist <- distribute
      optiX <- c(optiX, run)
      optiY <- c(optiY, globalmin)
      message("Burnin:", run, "  Min:", globalmin, "\n")
    }
  }

  # 2. annealing phase with pairwise reshuffling
  for (run in 1:length(annealprotocol)) {
    if (run %% 1000 == 0) {
      message("At iter", run, "\n")
    }

    # switch pairs
    distribute <- bestdist
    idx2 <- 1:length(distribute)
    r1 <- sample(idx2, annealprotocol[run])
    r2 <- sample(setdiff(idx2, r1), annealprotocol[run])
    tmp <- distribute[r1]
    distribute[r1] <- distribute[r2]
    distribute[r2] <- tmp

    shuffle[!fixed] <- distribute # update shuffle vector

    # scoring
    mfac <- cbind(grid, design[shuffle, ])
    penalty <- getScore(
      layout = mfac, balance = balance, sc_groups = scoring_groups,
      sc_tests = scoring_tests, bal_weights = balance_weights,
      sc_weights = scoring_weights
    )

    # update results
    if (penalty < globalmin) {
      globalmin <- penalty
      bestsol <- shuffle
      bestdist <- distribute
      optiX <- c(optiX, burnin + run)
      optiY <- c(optiY, globalmin)
      cat("Iter:", burnin + run, "  Min:", globalmin, "\n")
    }
  }

  cat("End of optimization\n")

  # formatting results
  # setcolorder(mfac,c(report,setdiff(colnames(mfac),report)))

  if (length(fixedPos) > 0) {
    if (length(convert) > 0) {
      # reconvert factors to letters # TODO: This version still needs testing
      mfac <- mfac %>%
        dplyr::mutate_at(.vars = dplyr::vars(convert), .funs = function(x) {
          factor(x, levels = 1:max(x, na.rm = TRUE), labels = LETTERS[1:max(x, na.rm = TRUE)])
        })
    }
  }
  return(list(
    design = mfac, globalmin = globalmin, bestdist = bestdist,
    opti = cbind(iteration = optiX, score = optiY)
  ))
}
