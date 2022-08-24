test_that("n_steps cannot be negative", {
  expect_error(OptimizationTrace$new(-10, 10))
})

test_that("n_scores cannot be negative", {
  expect_error(OptimizationTrace$new(10, -10))
})

test_that("OptimiztionTrace returns a single-column score matrix", {
  ot <- OptimizationTrace$new(10, 1, "")
  ot$set_scores(1, 2, NULL)
  ot$set_scores(2, 3, NULL)
  ot$shrink()
  expect_true(all(ot$scores == matrix(c(2, 3), ncol = 1)))
  expect_null(ot$aggregated_scores)
})

test_that("OptimiztionTrace fails on incorrect number of aggregated scores", {
  ot <- OptimizationTrace$new(10, 1, "")
  ot$set_scores(1, 2, c(2, 4))
  expect_error(ot$set_scores(2, 3, 3))
})

test_that("OptimiztionTrace returns a correct matrix of aggregated scores", {
  ot <- OptimizationTrace$new(10, 1, "a")
  ot$set_scores(1, 2, c(2, 4))
  ot$set_scores(2, 2, c(4, 2))
  ot$shrink()
  expect_true(all(ot$aggregated_scores == matrix(c(2, 4, 4, 2), ncol = 2)))
})

test_that("OptimiztionTrace returns a tibble", {
  ot <- OptimizationTrace$new(10, 1, "a")
  ot$set_scores(1, 2, c(2, 4))
  ot$set_scores(2, 2, c(4, 2))
  ot$shrink()
  tb <- ot$as_tibble()
  expect_true(tibble::is_tibble(tb))
  expect_named(tb, c("type", "step", "score", "value"))
  expect_true(all(tb$step %in% c(1, 2)))
  expect_true(is.factor(tb$type))
  expect_equal(levels(tb$type), c("score", "aggregated"))
  expect_true(all(tb$score %in% c("a", "agg.1", "agg.2")))
  expect_equal(nrow(tb), 6)
})

test_that("OptimiztionTrace returns a tibble (include_aggregated = FALSE)", {
  ot <- OptimizationTrace$new(10, 1, "a")
  ot$set_scores(1, 2, c(2, 4))
  ot$set_scores(2, 2, c(4, 2))
  ot$shrink()
  tb <- ot$as_tibble(FALSE)
  expect_true(tibble::is_tibble(tb))
  expect_named(tb, c("type", "step", "score", "value"))
  expect_true(all(tb$step %in% c(1, 2)))
  expect_true(is.factor(tb$type))
  expect_equal(levels(tb$type), c("score", "aggregated"))
  expect_true(all(tb$type == "score"))
  expect_true(all(tb$score == "a"))
  expect_equal(nrow(tb), 2)
})
