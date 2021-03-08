
# ---------------------------------------------------------------------------------
# This file contains some experimental prototypes for
# - scoring the isotropic distribution of (grouped) samples on a 2D plate
# - basic simulated annealing (SA) framework to optimize, can be certainly improved
# Find examples at the end of this file
# ---------------------------------------------------------------------------------

library(tidyverse)

mk_dist_matrix = function(plate_x=12, plate_y=8, dist="euclidean") {
  # Helper function: Sets up euclidean distance matrix for a generic x*y plate
  # distance function an be 'manhattan', otherwise all choises result in euclidean distance
  x_from_pos = function(pos) {
    floor((pos-1) / plate_y)+1
  }
  y_from_pos = function(pos) {
    floor((pos-1) %% plate_y)+1
  }

  n_pos = plate_x*plate_y
  dm = matrix(NA,nrow=n_pos, ncol=n_pos)
  for (i in 1:n_pos) {
    for (j in i:n_pos) {
      if (dist=="manhattan") {
        dm [i,j] = dm[j,i] = abs(x_from_pos(i)-x_from_pos(j)) +
          abs(y_from_pos(i)-y_from_pos(j))
      } else {
        dm [i,j] = dm[j,i] = (x_from_pos(i)-x_from_pos(j))^2 +
          (y_from_pos(i)-y_from_pos(j))^2
      }
    }
  }
  if (dist=="manhattan") {
    return(dm)
  }
  sqrt(dm)
}


# Basic skeleton for simulated annealing
# From https://gist.github.com/robertness/e69127ed752ef78f78db
# Note that all called sub-functions were missing from that resource,
# Also, fixed lots of typos and fliped energy scale to search for a maximum (distance) score
simulatedAnnealing <- function(s0, kMax, eMax, getNeighbor, getEnergy,
                               getAcceptanceProb, getTemp){
  #s0: initial state
  #kMax: Max desired number of iterations
  #eMax: Max desired energy
  #getNeighbor, getEnergy, getAcceptanceProb: neighborhood, energy, and
  #acceptance prob functions, respectively
  #Returns a list with the best state, the number of iterations reached
  #before annealing stopped, and the lowest energy when stopped.

  s <- s0
  e <- -getEnergy(s)
  sBest <- s
  eBest <- e
  k <- 1
  while(k <= kMax && e > eMax){
    temp <- getTemp(k)
    sNew <- getNeighbor(s)
    eNew <- -getEnergy(sNew)
    prob <- getAcceptanceProb(e, eNew, temp)
    if (is.na(prob)) {
      stop("Hahaha: e=",e,"  eNew = ",eNew,"  temp=",temp,"\n")
    }
    if(prob > runif(1)){
      s <- sNew
      e <- eNew
    }
    if(eNew < eBest){
      sBest <- sNew
      eBest <- eNew
      cat(-eBest,"at iter",k,"\n")

    }
    k <- k + 1
  }
  list(state = sBest, iterations = k, final.energy = -eBest)
}

mk_scoring_function = function(distance_matrix, initial_samplevec) {
  # A function factory returning one specific scoring function
  # This may be called in any optimizer
  force(distance_matrix) # needed?
  n_group = max(abs(initial_samplevec))

  # allocate memory for group scores
  group_scores = double(n_group)
  loopover = seq_along(group_scores)

  function(trial) {
    # Generates score for one specific sample arrangement (trial)
    # Omitting the explicit t() operation for the first argument saves
    #10% of CPU time.... :-|
    for (i in loopover) {
      group_trial = ifelse(abs(trial)==i,sign(trial),0)
      group_scores[i] = group_trial %*% distance_matrix %*% group_trial
    }
    min(group_scores)
  }

}

mk_neighbor_function = function(initial_samplevec) {
  # Function factory for creator of a neighboring sample arrangement vector
  # Currently always swaps to elements of the vector
  # (use in simulated annealing)

  pos = seq_along(initial_samplevec)

  function(sample_vec) {
    swap = sample(pos,2)
    sample_vec[swap] = sample_vec[rev(swap)]
    sample_vec
  }
}

acceptance_func = function(e, eNew, temp) {
  # for simulated annealing
  # returns a probability that a worse solution is accepted.
  # Selected according to default function in
  # https://en.wikipedia.org/wiki/Simulated_annealing
  # but should stay fixed probably and could be included in the SA function to save one frequent function call

  # Attempt to save a few % by avoiding the if statement :-) May fail for small temp because of numerical issues
  # (eNew<e) + (eNew>=e)*exp((e-eNew)/temp)

  if (eNew<e) {
    return(1)
  }
  exp((e-eNew)/temp)


}


temp_func = function(k, t0 = 50000, alpha=0.1) {
  # for simulated annealing
  # cooling schedule: returns temperature as function of the step number
  # Should probably re-parameterized so that k is running from 0 to 1 --> depending on maxiter, cooling can be slowed down.
  # Also, T0 and alpha should be converted to passed parameters
  # Uses  Quadratic multiplicative cooling as descried in
  # http://what-when-how.com/artificial-intelligence/a-comparison-of-cooling-schedules-for-simulated-annealing-artificial-intelligence/
  t0 /(1+alpha*k*k)
}

do_optimize = function(plate_x, plate_y, group_sizes=integer(), control_sizes=integer(),
                       maxiter=10000, dist="euclidean", optimizer="dumb") {

  # Generic optimizer, may use 'SA' or the dumb method which just randomly permutes the sample vector
  # Finds a good solution for the given plate size and group allocation

  # Construct initial sample vector and pad with 0s for wells thta stay empty
  # Matched controls are coded with negative integers, matching the correpsonding positive ones
  sample_vec= c( rep.int(seq_along(group_sizes), group_sizes),
                 rep.int(-seq_along(control_sizes), control_sizes),
                 rep.int(0,plate_x*plate_y-sum(group_sizes)-sum(control_sizes)) )

  dm = mk_dist_matrix(plate_x, plate_y, dist=dist)

  scoring_function = mk_scoring_function(dm, sample_vec)
  neighbor_function = mk_neighbor_function(sample_vec)

  if (optimizer=="SA") {
    # simulated annealing
    sa_result = simulatedAnnealing(s0 = sample_vec, kMax=maxiter,
                                   eMax=-Inf, # don't stop on that! Remove parameter or replace with a good convergence check
                                   getNeighbor=neighbor_function,
                                   getEnergy=scoring_function,
                                   getAcceptanceProb = acceptance_func,
                                   getTemp = temp_func)
    cat(sa_result$iterations,"iterations done.\n")
    best_trial = sa_result$state
    best_score = sa_result$final.energy

  } else {
    # The dumb optimizer :)
    best_score = -Inf
    iter =0
    while (iter<maxiter) {
      iter = iter+1
      trial = sample(sample_vec)
      score =  scoring_function(trial)
      if (score>best_score) {
        best_score = score
        best_trial = trial
        cat(score,"at iter", iter,"\n")
      }
    }
  }

  # Return samples arranged in plate layout
  matrix(best_trial, nrow = plate_y)

}

# ---------------------------------------------------------------------------------


# Example 1: An expensive way to construct a 4x4 latin square
# (latin square should give the best score)
# Highest score found with dumb optimizer is 29.78, using MANY iterations
start_time <- Sys.time()
do_optimize(plate_x = 4, plate_y = 4, group_sizes = c(4,4,4,4), maxiter=10000, optimizer = "SA") %T>%
  image() %>%
  print()
end_time <- Sys.time()
end_time - start_time

# Example 2: Two groups with 48 replications on a 96 well plate
# Very tough optimization problem --> should return a perfect checkerboard, ideally
start_time <- Sys.time()
do_optimize(plate_x = 12, plate_y = 8, group_sizes = c(48,48), maxiter=200000, optimizer="SA") %T>%
  image() %>%
  print()
end_time <- Sys.time()
end_time - start_time

# Example 3: 22 groups with 4 replicates each (The Ophtha case) - one column is not used (technical controls!)
# SA really rocks here!" :-)
do_optimize(plate_x = 11, plate_y = 8, group_sizes = rep(4,22), maxiter = 10000, optimizer="SA") %T>%
  image() %>%
  print()

# Example 4: 3 groups with different number of replicates, many wells
# remain empty
do_optimize(plate_x = 6, plate_y = 6, group_sizes = c(8,6,10), maxiter = 100000,optimizer="SA") %T>%
  image() %>%
  print()

# Example 5: 2 groups with paired controls, center well stays empty
do_optimize(plate_x = 3, plate_y = 3, group_sizes=c(2,2), control_sizes= c(2,2), maxiter = 5000, optimizer="SA") %T>%
  image() %>%
  print()

# Example 6: 2 groups, only one of them has paired controls (but many!)
# --> High repulsion between controls push group 1 into the middle of
# the plate
do_optimize(plate_x = 4, plate_y = 4, group_sizes=c(4,4), control_sizes= c(8), maxiter = 100000,optimizer="SA") %T>%
  image() %>%
  print()

# Example 7: Lots of space on the plate; 1 group has matched controls
# Optimal score found with dumb optimizer: 95.77
do_optimize(plate_x = 8, plate_y = 12, group_sizes=c(4,4,4,4), control_sizes = c(8), maxiter = 100000,optimizer="SA") %T>%
  image() %>%
  print()


