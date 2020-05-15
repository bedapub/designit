################################################
# randomization functions
################################################


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
countGini <- function(m){
  lev <- nlevels(m)
  m <- table(m, useNA = 'no')
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
meanDiff <- function(m){
  (max(m) - min(m))/mean(m)
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
kruskal <- function(m){
  m <- na.omit(m)
  x <- as.numeric(1:length(m))
  if (length(unique(m)) == 1) {
    kw <- as.double(length(levels(m))) # everything from the same group -> some large value
  } else {
    kw <- kruskal.test(x, m)$statistic
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
neighbors <- function(m){
  x <- sum(m[1:(length(m) - 1)] == m[2:length(m)])
  return(x)
}



#' Summary score for an experimental layout based on the distribution of levels
#' of the factors to be balanced.
#' @title get Score
#' @param layout a data.table with the factors to be balanced and their current positions
#' @param bal a vector with the names of experimental conditions to be balanced
#' @param sc_Weights named vector of weights for the dimensions (default all 1)
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
getScore <- function(layout = mfac, bal = balance, sc_groups = scoringGroups,
                     sc_tests = scoringTests, bal_weights = balweights,
                     sc_weights = scoringWeights) {

  scoresDim <- c() # vector to store the score of each dimension group
  for (s in 1:length(sc_groups)) {
    tscore <- 0
    group <- sc_groups[[s]]
    tests <- sc_tests[[s]]
    for (ti in 1:length(tests)) {
      # make test function from given string
      testFun <- function(x){do.call(tests[[ti]], list(x))}

      if (tests[[ti]] != 'meanDiff') {
        # get the penalty for each dimension group
        # factor von balace variables
        penalty <- layout[, lapply(.SD, testFun), .SDcols = bal, by = group]
        score <- penalty[, lapply(.SD, function(x){sum(x^2, na.rm = TRUE)}), .SDcols = bal]
        score <- sum(score * bal_weights) #multiply score for each balance factor by its weight and sum
        tscore <- tscore + score
      } else {
        # loop over balance variables
        means <- layout[, lapply(.SD, mean), .SDcols = bal, by = group]
        #uselevel <- ifelse(length(groups)>1, group[2])
        diff <- means[, lapply(.SD, meanDiff), .SDcols = bal]
        score <- sum(diff * bal_weights) #multiply score for each balance factor by its weight and sum
        tscore <- tscore + score
      }

    }
    scoresDim <- c(scoresDim, tscore)
  }
  return(sum(scoresDim * sc_weights) )
}


#' This function generates a randomized experimental layout based on given
#' experimental dimensions and factors to be balanced
#'
#' @title randomize experimental layout
#' @param design a data.frame with the sample ids, experimental conditions and
#' information about fixed samples (columns with 'Fix' prefix and then the dimension name)
#' @param report a string with the sample identifier
#' @param layoutDim a named vector with the experimental dimensions
#' @param balance a vector with the names of experimental conditions to be balanced
#' @param scoringGroups list of dimension name groups to be used jointly for scoring
#' @param scoringTests list of dimension name groups to be used jointly for scoring
#' @param burnin a number of initial burnin runs (default=100)
#' @param annealprotocol a vector with the number of pairs to swap in each annealing step
#' @param scoringWeights named vector of weights for the dimensions (default all 1)
#' @param balweights named vector of weights for the factors to be balanced (default all 1)
#' @param distribute a starting distribution
#'
#' @return the value of the Gini index
#'
#' @author Juliane Siebourg-Polster
#'
#' @export
#'
randomize <- function(design, report, layoutDim, balance,
                    factors = balance,
                    #scorings
                    scoringGroups = as.list(names(layoutDim)),
                    scoringTests = as.list(rep('countGini', length(layoutDim))),
                    #anneling params
                    burnin = 100, annealprotocol,
                    #weights
                    scoringWeights = rep(1,length(scoringGroups)),
                    balweights = rep(1,length(balance)),
                    #initial sample distribution
                    distribute = 1:(prod(layoutDim))){
  require(lattice)
  require(plyr)
  require(data.table)

  # initialization
  nloc <- prod(layoutDim) # available locations
  nsamp <- nrow(design)   # number of samples
  ndim <- length(layoutDim) # number of experiment dimensions
  grid <- data.table(expand.grid(sapply(layoutDim,
                                        function(x){seq(1 ,x)}, simplify = F))) # TODO: maybe lapply?

  shuffle <- rep.int(0, nloc) # vectors to track fixed fixedPos
  fixed <- rep(FALSE, nloc)

  # # make factors
  # design[, (factors) := lapply(.SD, factor), .SDcols = factors]
  for (f in balance) {design[,f] = factor(design[,f])}

  # Adding empty samples
  if (nloc < nsamp) {stop('too many samples')
  } else if (nloc > nsamp) {
    nempty <- nloc - nsamp
    cat('\n Adding', nempty,'empty sample(s) to fill layout\n')
    design[(nsamp + 1):nloc, ] <- matrix(NA, ncol = ncol(design), nrow = nempty)
  }


  # get fixed position columns
  fixedPos <- design[, grep('Fix', colnames(design))]
  if (length(fixedPos) > 0) {
    # convert to numeric
    convert <- colnames(fixedPos)[sapply(fixedPos, is.character)]
    if (length(convert) > 0) {
      fixedPos[, convert] <- lapply(
        convert, function(col){dim <- sub('Fix','',col);
                              fixedPos[, col] <- toupper(fixedPos[, col]);
                              fixedPos[, col] <- mapvalues(fixedPos[, col],
                                                         from = LETTERS[1:layoutDim[dim]],
                                                         to = 1:layoutDim[dim]);
                              as.numeric(fixedPos[, col])})
    }
    # put columns in same order as layoutDim
    fixedPos <- fixedPos[, paste0('Fix', names(layoutDim))]

    # fixed position index vector
    if (ncol(fixedPos) == length(layoutDim)) {
        # fixed position indicator
        f <- apply((!is.na(fixedPos)), 1, all)
        # fixed vector index
        idxf <- apply(fixedPos[which(f), 1:ndim], 1,
                      function(pos){sum((pos - c(0, rep(1, ndim - 1))) *
                                          cumprod(c(1, layoutDim[1:(ndim - 1)])))})
        fixed[idxf] <- TRUE
        shuffle[idxf] <- (1:nloc)[f]
        if (length(distribute) == length(f)) {
            distribute <- distribute[!f]
        }
    }
    # TODO: add something for reusable layout, i.e. same fixed rows / cols per each plate
  }

  optiX <- optiY <- vector() # vectors to track optimization
  shuffle[!fixed] <- distribute #update shuffle vector

  bestdist <- distribute
  bestsol <- shuffle


  # sort balance factors by current layout and get penalty score
  mfac <- data.table(grid,design[shuffle, ])
  globalmin <- getScore(layout = mfac, bal = balance, sc_groups = scoringGroups,
                        sc_tests = scoringTests, bal_weights = balweights,
                        sc_weights = scoringWeights)
  cat('start optimization from minimum:', globalmin,'\n')


  # Start of optimization

  # 1. 'burnin' phase with complete reshuffling
  for (run in 1:burnin) {
    # new distribution + update shuffle vector
    distribute <- sample(distribute)
    shuffle[!fixed] <- distribute

    # scoring
    mfac <- data.table(grid,design[shuffle,])
    penalty <- getScore(layout = mfac, bal = balance, sc_groups = scoringGroups,
                      sc_tests = scoringTests, bal_weights = balweights,
                      sc_weights = scoringWeights)

    # update results
    if (penalty < globalmin) {
      globalmin <- penalty
      bestsol <- shuffle
      bestdist <- distribute
      optiX <- c(optiX, run)
      optiY <- c(optiY, globalmin)
      cat("Burnin:",run,"  Min:", globalmin,"\n")
    }
  }

  # 2. annealing phase with pairwise reshuffling
  for (run in 1:length(annealprotocol)) {

    if (run %% 1000 == 0) { cat("At iter", run, "\n")}

    # switch pairs
    distribute <- bestdist;
    idx2 <- 1:length(distribute)
    r1 <- sample(idx2,annealprotocol[run])
    r2 <- sample(setdiff(idx2,r1), annealprotocol[run])
    tmp <- distribute[r1]
    distribute[r1] <- distribute[r2]
    distribute[r2] <- tmp

    shuffle[!fixed] <- distribute #update shuffle vector

    # scoring
    mfac <- data.table(grid,design[shuffle,])
    penalty <- getScore(layout = mfac, bal = balance, sc_groups = scoringGroups,
                      sc_tests = scoringTests, bal_weights = balweights,
                      sc_weights = scoringWeights)

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
      mfac[, (convert) := lapply(.SD, function(x){
        mapvalues(x, from = 1:max(x, na.rm = TRUE), to = LETTERS[1:max(x,na.rm = TRUE)])
      }), .SDcols = convert]
      # for(col in convert){
      #   dim <- sub('Fix', '', col)
      #   mfac[, dim,with=FALSE] <- mapvalues(mfac[,dim,with=FALSE],
      #                                       from=1:layoutDim[dim], to=LETTERS[1:layoutDim[dim]])
      # }
    }
  }
  return(list(design = mfac, globalmin = globalmin, bestdist = bestdist,
              opti = cbind(iteration = optiX, score = optiY)))

}

