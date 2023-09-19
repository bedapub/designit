set.seed(42)

bc <- BatchContainer$new(
  dimensions = c(
    plate = 2,
    row = 4, col = 4
  )
)

bc <- assign_in_order(bc, samples = tibble::tibble(
  Group = c(rep(c("Grp 1", "Grp 2", "Grp 3", "Grp 4"), each = 8)),
  ID = 1:32
))

# here we use a 2-step approach:
# 1. Assign samples to plates.
# 2. Arrange samples within plates.

# overview of sample assagnment before optimization
plot_plate(bc,
  plate = plate, row = row, column = col, .color = Group
)

# Step 1, assign samples to plates
scoring_f <- osat_score_generator(
  batch_vars = c("plate"), feature_vars = c("Group")
)
bc <- optimize_design(
  bc,
  scoring = scoring_f,
  max_iter = 10, # the real number of iterations should be bigger
  n_shuffle = 2,
  quiet = TRUE
)
plot_plate(
  bc,
  plate = plate, row = row, column = col, .color = Group
)

# Step 2, distribute samples within plates
scoring_f <- mk_plate_scoring_functions(
  bc,
  plate = "plate", row = "row", column = "col", group = "Group"
)
bc <- optimize_design(
  bc,
  scoring = scoring_f,
  max_iter = 50,
  shuffle_proposal_func = mk_subgroup_shuffling_function(subgroup_vars = c("plate")),
  aggregate_scores_func = L2s_norm,
  quiet = TRUE
)
plot_plate(bc,
  plate = plate, row = row, column = col, .color = Group
)
