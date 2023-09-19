test_that("basic all_equal_df behavior", {
  expect_true(all_equal_df(iris, iris))

  x <- iris
  expect_true(all_equal_df(x, iris))
  # add some NAs
  x[1, 2] <- 20
  expect_false(all_equal_df(x, iris))

  # add some NAs
  x <- iris
  x[1, 1] <- NA
  expect_false(all_equal_df(x, iris))
})

test_that("all_equal_df on empty dfs", {
  expect_true(all_equal_df(data.frame(), data.frame()))
  expect_true(all_equal_df(
    data.frame(a=character(0), b=integer(0)),
    data.frame(a=character(0), b=integer(0))
  ))
})

test_that("all_equal_df on NA dfs", {
  expect_true(all_equal_df(
    data.frame(a=rep(NA_character_,10), b=rep(NA_integer_, 10)),
    data.frame(a=rep(NA_character_,10), b=rep(NA_integer_, 10))
  ))
  x <- data.frame(a=rep(NA_character_,10), b=rep(NA_integer_, 10))
  y <- x
  x[3, "b"] = 2
  expect_false(all_equal_df(x, y))
})


test_that("all_equal_df on reorder", {
  for (i in 1:10) {
    row_reorder <- iris[sample(seq_len(nrow(iris))),]
    expect_true(all_equal_df(iris, row_reorder))
  }

  for (i in 1:10) {
    col_reorder <- iris[sample(colnames(iris))]
    expect_true(all_equal_df(iris, col_reorder))
  }

  for (i in 1:10) {
    row_col_reorder <- iris[sample(seq_len(nrow(iris))),]
    row_col_reorder <- row_col_reorder[sample(seq_len(nrow(row_col_reorder))),]
    expect_true(all_equal_df(iris, row_col_reorder))
  }
})

test_that("all_equal_df on reorder with NAs", {
  x <- data.frame(a=rep(NA_character_,10), b=rep(NA_integer_, 10))
  x[3, "b"] = 2

  for (i in 1:10) {
    row_col_reorder <- x[sample(seq_len(nrow(x))),]
    row_col_reorder <- row_col_reorder[sample(seq_len(nrow(row_col_reorder))),]
    expect_true(all_equal_df(x, row_col_reorder))
  }
})

test_that("compare tibble and a data.frame with all_equal_df", {
  x <- tibble::tibble(iris)
  expect_true(all_equal_df(x, iris))
})
