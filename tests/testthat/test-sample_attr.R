bc <- BatchContainer$new(
  dimensions = c(row = 3, column = 3)
)
samp <- data.frame(i = 1:8)
assign_in_order(bc, samp)

test_that("cannot assign has_sample_attributes", {
  bc1 <- bc$copy()
  expect_false(bc1$has_sample_attributes)
  expect_error(bc1$has_sample_attributes <- TRUE)
  expect_false(bc1$has_sample_attributes)
})

test_that("can assign and retrive sample_attr", {
  bc1 <- bc$copy()
  expect_equal(nrow(bc1$samples_attr), 0)
  expect_false(bc1$has_sample_attributes)
  expect_error(bc1$samples_attr <- data.frame(new_val=1:3))
  bc1$samples_attr <- data.frame(new_val=seq_len(nrow(bc1$samples)))
  expect_true(bc1$has_sample_attributes)
  expect_true(any('new_val' %in% names(bc1$get_samples())))
})

test_that("can retrive sample_attr after copying", {
  bc1 <- bc$copy()
  bc2 <- bc1$copy()
  bc1$samples_attr <- data.frame(new_val=seq_len(nrow(bc1$samples)))
  samples_tab <- bc1$get_samples()
  expect_true(bc1$has_sample_attributes)
  expect_true(any('new_val' %in% names(bc1$get_samples())))
  expect_false(bc2$has_sample_attributes)
  expect_false(any('new_val' %in% names(bc2$get_samples())))
  bc3 <- bc1$copy()
  expect_true(bc3$has_sample_attributes)
  expect_true(any('new_val' %in% names(bc3$get_samples())))
})
