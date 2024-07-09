samples <- data.frame(i = 1:384, x = rnorm(384))

bc <- BatchContainer$new(
  dimensions = c(row = 16, column = 24)
) %>%
  assign_in_order(samples)


test_that("scoring functions can be an unnamed list", {
  tmp <- optimize_design(bc,
    scoring = list(
      \(...) rnorm(1),
      \(...) rnorm(1)
    ),
    max_iter = 10
  )
  expect_equal(
    colnames(tmp$trace$scores[[1]]),
    c("step", "score_1", "score_2")
  )
})

test_that("all scoring functions should be named (or unnamed)", {
  expect_error(
    optimize_design(bc,
      scoring = list(
        \(...) rnorm(1),
        f = \(...) rnorm(1)
      ),
      max_iter = 10
    ),
    "scoring cannot be a partially named list"
  )
})

test_that("scoring function cannot have a name 'step'", {
  expect_error(
    optimize_design(bc,
      scoring = list(
        step = \(...) rnorm(1)
      ),
      max_iter = 10
    ),
    "score name cannot be 'step'"
  )
})
