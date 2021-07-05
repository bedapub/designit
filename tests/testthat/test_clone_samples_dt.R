test_that("$clone() does not preserve $samples_dt", {
  bc <- BatchContainer$new(
    dimensions = c(row = 3, column = 3)
  )
  samp <- data.frame(i = 1:9)
  assign_in_order(bc, samp)
  # creates cache
  bc$samples_dt
  bc_clone <- bc$clone()
  bc_clone$exchange_samples(c(1L, 2L), c(2L, 1L))

  expect_equal(bc$samples_dt$.sample_id, 1:9)
})
