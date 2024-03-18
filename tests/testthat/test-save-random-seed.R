bc <- BatchContainer$new(
  dimensions = c(location = 20)
) |>
  assign_in_order(
    data.frame(
      sampleId = seq_len(20)
    )
  )

scoring_f <- function(...) rnorm(1)

test_that("random seed is saved when no seed is set", {
  # unset random seed
  if (exists(".Random.seed")) {
    random_seed <- .Random.seed
    on.exit(.GlobalEnv$.Random.seed <- random_seed)
    rm(.Random.seed, envir = .GlobalEnv)
    testthat::expect_false(exists(".Random.seed"))
  }
  
  bc_opt <- optimize_design(
    bc,
    scoring = scoring_f,
    max_iter = 3
  )
  expect_s3_class(bc_opt$trace, "data.frame")
  expect_equal(nrow(bc_opt$trace), 1)
  expect_type(bc_opt$trace$seed, "list")
  expect_type(bc_opt$trace$seed[[1]], "integer")
  expect_gt(length(bc_opt$trace$seed[[1]]), 0)
  expect_type(bc_opt$trace$rng_kind, "list")
  expect_type(bc_opt$trace$rng_kind[[1]], "character")
  expect_length(bc_opt$trace$rng_kind[[1]], 3)
})


test_that("random seed is saved when it was set", {
  set.seed(6)
  bc_opt <- optimize_design(
    bc,
    scoring = scoring_f,
    max_iter = 3
  )
  expect_s3_class(bc_opt$trace, "data.frame")
  expect_equal(nrow(bc_opt$trace), 1)
  expect_type(bc_opt$trace$seed, "list")
  expect_type(bc_opt$trace$seed[[1]], "integer")
  expect_gt(length(bc_opt$trace$seed[[1]]), 0)
  expect_type(bc_opt$trace$rng_kind, "list")
  expect_type(bc_opt$trace$rng_kind[[1]], "character")
  expect_length(bc_opt$trace$rng_kind[[1]], 3)
})
