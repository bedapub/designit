
# ---------------------------------------------------------------------------------
# This file contains some experimental prototypes for
# - scoring the isotropic distribution of (grouped) samples on a 2D plate
# - basic simulated annealing (SA) framework to optimize, can be certainly improved
# - an experimental addition to SA that allows to combine annealing with a dynamic shuffling protocol
# Find examples at the end of this file
# ---------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)

# ---------------------------------------------------------------------------------
# Simulated annealing and related helper functions
# ---------------------------------------------------------------------------------

swap_protocol_constant = function(n=1) {
  # Function factory: Returns a function that causes n sample swaps per iteration, independent from annealing temperature
  function(temp) {
    n
  }
}

swap_protocol_anneal = function(temp) {
  # Number of sample position swaps per iteration as a function of the annealing temperature
  # With this model, decrease number of swaps with temperature
  floor(log(temp+1))+1
}

swap_protocol_endboost = function(temp) {
  # Number of sample position swaps per iteration as a function of the annealing temperature
  # With this model, do 1 swap per iteration except a swapping boost with peak at very low temperature (T=0.5),
  # trying to avoid local minimum
  floor(10*dnorm(temp, 0.5, 0.1))+1
}


mk_neighbor_function = function(initial_samplevec) {
  # Function factory for creator of a 'neighboring' sample arrangement with a defined number of position swaps

  pos = seq_along(initial_samplevec)
  lehalf = floor(length(pos)/2)

  function(sample_vec, nr_swaps=1) {
    if (nr_swaps>lehalf) { # Limit swaps to length of position vector
      nr_swaps=lehalf
    }
    swap = sample(pos,2*nr_swaps)
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



# Basic skeleton for simulated annealing
# From https://gist.github.com/robertness/e69127ed752ef78f78db
# Note that all called sub-functions were missing from that resource,
# Also, fixed lots of typos and flipped energy scale to search for a maximum (distance) score
# Added function parameter swap_protocol to allow a more dynamic protocol for shuffling trial
# solutions by swapping sample positions. Defaults to making one swap per iteration.

simulatedAnnealing <- function(s0, kMax, eMax, getNeighbor, getEnergy,
                               getAcceptanceProb, getTemp,
                               swap_protocol = swap_protocol_constant(1)){
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
  cat(-eBest,"at iter 0\n")

  k <- 1
  while(k <= kMax && e > eMax){
    temp <- getTemp(k)
    nr_swaps = swap_protocol(temp)
    sNew <- getNeighbor(s, nr_swaps)
    eNew <- -getEnergy(sNew)
    prob <- getAcceptanceProb(e, eNew, temp)
    if(prob > runif(1)){
      s <- sNew
      e <- eNew
    }
    if(eNew < eBest){
      sBest <- sNew
      eBest <- eNew
      cat(-eBest,"at iter",k,"  swaps:",nr_swaps," temp:",temp,"\n")

    }
    k <- k + 1
  }
  list(state = sBest, iterations = k, final.energy = -eBest)
}


# ---------------------------------------------------------------------------------
# General helper functions
# ---------------------------------------------------------------------------------


mk_dist_matrix = function(plate_x=12, plate_y=8, dist="euclidean") {
  # Helper function: Sets up a euclidean or alternative distance matrix (supported by stats::dist) for a generic x*y plate
  matrix( c( rep(1:plate_y,plate_x), rep(1:plate_x, each=plate_y)), ncol = 2 ) %>%
    stats::dist(method = dist) %>%
    as.matrix()
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

# ---------------------------------------------------------------------------------
# Optimizer
# ---------------------------------------------------------------------------------

do_optimize = function(plate_x=NA, plate_y=NA, group_sizes=integer(),
                       control_sizes=integer(),
                       maxiter=10000,
                       dist="euclidean",
                       optimizer="dumb",
                       swap_protocol=swap_protocol_constant(1)) {

  # Generic optimizer, may use 'SA' or the dumb method which just randomly permutes the sample vector
  # Finds a good solution for the given plate size and group allocation

  # Construct initial sample vector and pad with 0s for wells that stay empty
  # Matched controls are coded with negative integers, matching the corresponding positive ones
  sample_vec= c( rep.int(seq_along(group_sizes), group_sizes),
                 rep.int(-seq_along(control_sizes), control_sizes) )
  if (!is.na(plate_x) && !is.na(plate_y)) {
    sample_vec = c(sample_vec, rep.int(0,plate_x*plate_y-sum(group_sizes)-sum(control_sizes)) )
  }

  if (any(class(dist)=="matrix")) {
    stopifnot( all(dim(dist)==length(sample_vec)) ) # wrong size of passed matrix!
    dm = dist # user has passed a custom distance matrix anyway
  } else {
    dm = mk_dist_matrix(plate_x, plate_y, dist=dist)
  }

  scoring_function = mk_scoring_function(dm, sample_vec)
  neighbor_function = mk_neighbor_function(sample_vec)

  if (optimizer=="SA") {

    sa_result = simulatedAnnealing(s0 = sample_vec, kMax=maxiter,
                                   eMax=-Inf, # don't ever stop on that!
                                   getNeighbor=neighbor_function,
                                   getEnergy=scoring_function,
                                   getAcceptanceProb = acceptance_func,
                                   getTemp = temp_func,
                                   swap_protocol = swap_protocol)
    cat(sa_result$iterations,"iterations done.\n")
    best_trial = sa_result$state
    best_score = sa_result$final.energy

  } else {
    # Replace the following random permutation loop with a better
    # optimization strategy, i.e. by dynamically adjusting number of swaps
    best_score = -Inf
    iter =0
    trial = sample_vec # start with initial setup so that one can try to pass the optimal solution as a test

    while (iter<maxiter) {
      iter = iter+1
      score = scoring_function(trial)
      if (score>best_score) {
        best_score = score
        best_trial = trial
        cat(score,"at iter", iter,"\n")
      }
      trial = sample(sample_vec)
    }
  }


  matrix(best_trial, nrow = ifelse(is.na(plate_y), 1, plate_y))

}


# ---------------------------------------------------------------------------------


# # Example 1: An expensive way to construct a 4x4 latin square
# # (latin square should give the best score)
# # Highest score found with dumb optimizer is 29.78, using MANY iterations
# start_time <- Sys.time()
# do_optimize(plate_x = 4, plate_y = 4, group_sizes = c(4,4,4,4), maxiter=10000,
#             optimizer = "SA", swap_protocol = swap_protocol_constant(2)) %T>%
#   image() %>%
#   print()
# end_time <- Sys.time()
# end_time - start_time
#
# # Example 2: Two groups with 48 replications on a 96 well plate
# # Very tough optimization problem --> should return a perfect checkerboard, ideally
# # Optimal score is 12063.82!
# start_time <- Sys.time()
# do_optimize(plate_x = 12, plate_y = 8, group_sizes = c(48,48), maxiter=200000,
#             optimizer="SA", swap_protocol = swap_protocol_constant(5)) %T>%
#   image() %>%
#   print()
# end_time <- Sys.time()
# end_time - start_time
#
# # Example 3: 22 groups with 4 replicates each (The Ophtha case) - one column is not used (technical controls!)
# # SA really rocks here!" :-)
# do_optimize(plate_x = 11, plate_y = 8, group_sizes = rep(4,22), maxiter = 50000,
#             optimizer="SA", swap_protocol = swap_protocol_constant(3)) %T>%
#   image() %>%
#   print()
#
# # Example 4: 3 groups with different number of replicates, many wells
# # remain empty
# # 145.9548 is always (?) found as optimum with different configurations
# do_optimize(plate_x = 6, plate_y = 6, group_sizes = c(8,6,10), maxiter = 10000,
#             optimizer="SA") %T>%
#   image() %>%
#   print()
#
# # Example 5: 2 groups with paired controls, center well stays empty
# do_optimize(plate_x = 3, plate_y = 3, group_sizes=c(2,2), control_sizes= c(2,2), maxiter = 5000,
#             optimizer="SA") %T>%
#   image() %>%
#   print()
#
# # Example 6: 2 groups, only one of them has paired controls (but many!)
# # --> High repulsion between controls push group 1 into the middle of
# # the plate
# # Best score 35.4589
# do_optimize(plate_x = 4, plate_y = 4, group_sizes=c(4,4), control_sizes= c(8), maxiter = 100000,
#             optimizer="SA") %T>%
#   image() %>%
#   print()
#
# # Example 7: Lots of space on the plate; 1 group has matched controls
# # Optimal score found with dumb optimizer: 95.77, with SA 112.6508
# do_optimize(plate_x = 8, plate_y = 12, group_sizes=c(4,4,4,4), control_sizes = c(8), maxiter = 200000,
#             optimizer="SA", swap_protocol = swap_protocol_anneal) %T>%
#   image() %>%
#   print()
#
#
# # Juliane's example for a nested batch situation - need a different kind of distance matrix here,
# # reflecting the batch / run structure of the container, which is not a plate :)
#
# data("multi_trt_day_samples")
# grouping = count( multi_trt_day_samples, Treatment, Time)
#
# # Batch arrangement forms the container
# # Treat Batch and Run as an ordinal variable (consecutive days or time slots)
# container=tibble( Batch=paste("Batch",rep(1:4,each=8)),
#                   Run=paste("Run",rep(rep(1:2,each=4),4)),
#                   Position=rep(1:4,8)) %>%
#   mutate(across(all_of(c("Batch", "Run")), factor))
#
#
#
# # 1st try: make all groups independent, i.e. treatments and time points are just unrelated labels
#
# # Construct a distance matrix for the non-nested relation between Batch, Run and Position
# D1 = matrix(0.0, nrow = nrow(container), ncol=nrow(container))
# for (i in 1:nrow(container)) {
#   for (j in i:nrow(container)) {
#     D1[i,j] = D1[j,i] = abs(as.numeric(container$Batch[i]) -as.numeric(container$Batch[j]) ) +
#       abs(as.numeric(container$Run[i]) -as.numeric(container$Run[j]) ) +
#       0.2*abs(container$Position[i]- container$Position[j])
#   }
# }
# image(D1)
#
# sol = do_optimize(group_sizes=grouping$n, dist = D1, maxiter = 50000,
#             optimizer="SA", swap_protocol = swap_protocol_constant(2)) %>%
#   drop()
#
#
# design = bind_cols( container, tibble(Treatment=grouping$Treatment[sol], Time=grouping$Time[sol]) ) %>%
#   arrange(Treatment, Time) %>%
#   bind_cols( arrange(multi_trt_day_samples, Treatment, Time) %>% select(SampleName) )
#
#
# p1 = design %>%
#   ggplot(aes(x = Run, y=Position, fill =  Treatment, alpha = factor(Time))) +
#   geom_raster() +
#   facet_wrap(~Batch) +
#   scale_alpha_manual(values=c("4"=0.4, "8"=0.9)) +
#   geom_text(aes(label=Time)) +
#   theme(legend.position = "bottom") +
#   guides(alpha = FALSE)+
#   labs(subtitle='No nesting in factors Batch, Run and Position', fill="")
#
#
#
# # 2nd try: add nestings of the form Batch/Run and Batch/Position
#
# D2 = matrix(0.0, nrow = nrow(container), ncol=nrow(container))
# for (i in 1:nrow(container)) {
#   for (j in i:nrow(container)) {
#     D2[i,j] = D2[j,i] = abs(as.numeric(container$Batch[i]) -as.numeric(container$Batch[j]) ) +
#       abs(as.numeric(container$Run[i]) -as.numeric(container$Run[j]) ) *
#       (container$Batch[i]==container$Batch[j]) +
#       0.2*abs(container$Position[i]- container$Position[j]) *
#       (container$Batch[i]==container$Batch[j])
#   }
# }
# image(D2)
#
# sol = do_optimize(group_sizes=grouping$n, dist = D2, maxiter = 50000,
#                   optimizer="SA", swap_protocol = swap_protocol_constant(2)) %>%
#   drop()
#
#
# design = bind_cols( container, tibble(Treatment=grouping$Treatment[sol], Time=grouping$Time[sol]) ) %>%
#   arrange(Treatment, Time) %>%
#   bind_cols( arrange(multi_trt_day_samples, Treatment, Time) %>% select(SampleName) )
#
#
# p2 = design %>%
#   ggplot(aes(x = Run, y=Position, fill =  Treatment, alpha = factor(Time))) +
#   geom_raster() +
#   facet_wrap(~Batch) +
#   scale_alpha_manual(values=c("4"=0.4, "8"=0.9), guide=NA) +
#   geom_text(aes(label=Time)) +
#   theme(legend.position = "bottom") +
#   guides(alpha = FALSE)+
#   labs(subtitle='With nestings Batch/Run and Batch/Position',fill="")
#
# plot_grid(p1,p2,ncol=2)
