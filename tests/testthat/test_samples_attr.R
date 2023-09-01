bc <- BatchContainer$new(
  dimensions = c(row = 3, column = 3)
)
samp <- data.frame(i = 1:8)
bc$samples <- samp

test_that("add attributes before assigning samples", {
  bc$samples_attr <- data.frame(attr1 = rev(1:8))
  expect_error(bc$get_samples())
  expect_equal(bc$get_samples(assignment = FALSE)$attr1, rev(1:8))
})

bc <- assign_in_order(bc)

test_that("add attributes after assigning samples", {
  bc$samples_attr <- NULL
  expect_false("attr1" %in% colnames(bc$get_samples()))
  bc$samples_attr <- data.frame(attr1 = rev(1:8))
  expect_equal(bc$get_samples(assignment = FALSE)$attr1, rev(1:8))
  expect_equal(bc$get_samples(assignment = TRUE)$attr1, c(rev(1:8), NA))
})


test_that("shuffling samples keeps attributes order", {
  set.seed(42)
  bc <- assign_random(bc)
  bc$get_samples(include_id = TRUE, as_tibble = FALSE)
  bc$samples_attr <- data.frame(attr1 = rev(1:8))
  expect_equal(bc$get_samples(assignment = FALSE)$attr1, rev(1:8))
  stab <- bc$get_samples(assignment = TRUE, include_id = TRUE, as_tibble = FALSE)
  stab <- stab[order(stab$.sample_id), ]
  expect_equal(stab$attr1, c(rev(1:8), NA))
})

test_that("adding multiple sample attributes", {
  bc$samples_attr <- data.frame(attr1 = rev(1:8), attr2 = 1:8, attr3 = 1:8)
  expect_equal(colnames(bc$get_samples()), c("row", "column", "i", "attr1", "attr2", "attr3"))
})

test_that("deleting sample attributes", {
  bc$samples_attr <- data.frame(attr1 = rev(1:8), attr2 = 1:8, attr3 = 1:8)
  bc$samples_attr <- NULL
  expect_equal(colnames(bc$get_samples()), c("row", "column", "i"))
})
