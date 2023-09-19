bc <- BatchContainer$new(
  dimensions = c(row = 3, column = 3)
)
samp <- data.frame(i = 1:8)
bc <- assign_in_order(bc, samp)

test_that("cannot assign has_samples_attr", {
  bc1 <- bc$copy()
  expect_false(bc1$has_samples_attr)
  expect_error(bc1$has_samples_attr <- TRUE)
  expect_false(bc1$has_samples_attr)
})

test_that("can assign and retrieve sample_attr", {
  bc1 <- bc$copy()
  expect_equal(nrow(bc1$samples_attr), 0)
  expect_false(bc1$has_samples_attr)
  expect_error(bc1$samples_attr <- data.frame(new_val = 1:3))
  bc1$samples_attr <- data.frame(new_val = seq_len(nrow(bc1$samples)))
  expect_true(bc1$has_samples_attr)
  expect_true(any("new_val" %in% names(bc1$get_samples())))
})

test_that("can retrieve sample_attr after copying", {
  bc1 <- bc$copy()
  bc2 <- bc1$copy()
  bc1$samples_attr <- data.frame(new_val = seq_len(nrow(bc1$samples)))
  samples_tab <- bc1$get_samples()
  expect_true(bc1$has_samples_attr)
  expect_true(any("new_val" %in% names(bc1$get_samples())))
  expect_false(bc2$has_samples_attr)
  expect_false(any("new_val" %in% names(bc2$get_samples())))
  bc3 <- bc1$copy()
  expect_true(bc3$has_samples_attr)
  expect_true(any("new_val" %in% names(bc3$get_samples())))
})
