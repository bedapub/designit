bcd1 <- BatchContainerDimension$new("d1", 10)
bcd2 <- BatchContainerDimension$new("d2", values = letters[1:10])

bc1 <- BatchContainer$new(
  dimensions = list(bcd1, bcd2)
)
bc2 <- BatchContainer$new(
  dimensions = list(d1 = bcd1, d2 = bcd2, d3 = 1)
)
bc3_excl <- BatchContainer$new(
  dimensions = list(d1 = bcd1, d2 = bcd2),
  exclude = data.frame(d1 = 1, d2 = "a")
)
bc4_excl <- BatchContainer$new(
  dimensions = list(d1 = bcd1, d2 = bcd2, d3 = list(values = "x")),
  exclude = data.frame(d1 = 1:3, d2 = "a", d3 = "x")
)



test_that("Test that BatchContainer size is correct", {
  expect_equal(bc1$n_locations, 100)
  expect_equal(bc2$n_locations, 100)
  expect_equal(bc3_excl$n_locations, 100 - 1)
  expect_equal(bc4_excl$n_locations, 100 - 3)
})

test_that("Test that numer of dimensions is correct", {
  expect_equal(bc1$n_dimensions, 2)
  expect_equal(bc2$n_dimensions, 3)
  expect_equal(bc3_excl$n_dimensions, 2)
  expect_equal(bc4_excl$n_dimensions, 3)
})

test_that("Test locations", {
  expect_equal(nrow(bc1$get_locations()), bc1$n_locations)
  expect_equal(nrow(bc2$get_locations()), bc2$n_locations)
  expect_equal(nrow(bc3_excl$get_locations()), bc3_excl$n_locations)
  expect_equal(nrow(bc4_excl$get_locations()), bc4_excl$n_locations)
})

samples <- data.frame(a = 1:10, b = rnorm(10))

samples_with_na <- rbind(samples, data.frame(a = NA, b = NA))

test_that("Test adding samples and then assigning them", {
  bc1_copy <- bc1$copy()
  bc1_copy$samples <- samples
  expect_null(bc1_copy$assignment)
  bc1_copy <- assign_in_order(bc1_copy)
  expect_equal(
    bc1_copy$assignment,
    c(seq_len(nrow(samples)), rep(NA_integer_, bc1_copy$n_locations - nrow(samples)))
  )

  bc1_copy <- bc1$copy()
  bc1_copy <- assign_in_order(bc1_copy, samples)
  expect_equal(
    bc1_copy$assignment,
    c(seq_len(nrow(samples)), rep(NA_integer_, bc1_copy$n_locations - nrow(samples)))
  )
})


test_that("Test assigning samples randomly", {
  bc3_copy <- bc3_excl$copy()
  expect_null(bc3_copy$assignment)
  expect_false(any(!is.na(bc3_copy$assignment)))
  bc3_copy <- assign_random(bc3_copy, samples)
  expect_true(any(!is.na(bc3_copy$assignment)))
})

test_that("Test assigning too many or empty samples", {
  bc3_copy <- bc3_excl$copy()
  bc4_copy <- bc4_excl$copy()
  expect_error(bc3_copy$samples <- data.frame())
  expect_error(bc3_copy$samples <- data.frame(a = seq_len(bc3_copy$n_locations + 1)))
  expect_error(bc4_copy$samples <- data.frame())
  expect_error(bc4_copy$samples <- data.frame(a = seq_len(bc4_copy$n_locations + 1)))
})

test_that("Test double sample assignment", {
  bc3_copy <- bc3_excl$copy()
  bc3_copy$samples <- samples
  expect_error(bc3_copy$samples <- samples)
  expect_error(assign_in_order(bc3_copy, samples))
  expect_error(assign_random(bc3_copy, samples))
  bc4_copy <- bc4_excl$copy()
  bc4_copy$samples <- samples
  expect_error(bc4_copy$samples <- samples)
  expect_error(assign_in_order(bc4_copy, samples))
  expect_error(assign_random(bc4_copy, samples))
})

test_that("Test assignment of samples with all-NA attributes", {
  bc3_copy <- bc3_excl$copy()
  expect_error(bc3_copy$samples <- samples_with_na)
  expect_error(assign_in_order(bc3_copy, samples_with_na))
  expect_error(assign_random(bc3_copy, samples_with_na))
  bc4_copy <- bc4_excl$copy()
  expect_error(bc4_copy$samples <- samples_with_na)
  expect_error(bc4_copy$samples <- samples_with_na)
  expect_error(assign_in_order(bc4_copy, samples_with_na))
  expect_error(assign_random(bc4_copy, samples_with_na))
})
