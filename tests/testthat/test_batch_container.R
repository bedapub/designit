library(designit)


bcd1 <- BatchContainerDimension$new("d1", 10)
bcd2 <- BatchContainerDimension$new("d2", values = letters[1:10])

bc1 <- BatchContainer$new(
  dimensions = list(bcd1, bcd2)
)
bc2 <- BatchContainer$new(
  dimensions = list(d1=bcd1, d2=bcd2, d3=1)
)
bc3_excl <- BatchContainer$new(
  dimensions = list(d1=bcd1, d2=bcd2),
  exclude=data.frame(d1=1, d2="a")
)
bc4_excl <- BatchContainer$new(
  dimensions = list(d1=bcd1, d2=bcd2, d3=list(values="x")),
  exclude=data.frame(d1=1:3, d2="a", d3="x")
)



test_that("Test that BatchContainer size is correct", {
  expect_equal(bc1$n_locations, 100)
  expect_equal(bc2$n_locations, 100)
  expect_equal(bc3_excl$n_locations, 100)
  expect_equal(bc4_excl$n_locations, 100)

  expect_equal(bc1$n_available, 100)
  expect_equal(bc2$n_available, 100)
  expect_equal(bc3_excl$n_available, 100 - 1)
  expect_equal(bc4_excl$n_available, 100 - 3)

  expect_equal(bc1$n_excluded, 0)
  expect_equal(bc2$n_excluded, 0)
  expect_equal(bc3_excl$n_excluded, 1)
  expect_equal(bc4_excl$n_excluded, 3)
})

test_that("Test that numer of dimensions is correct", {
  expect_equal(bc1$n_dimensions, 2)
  expect_equal(bc2$n_dimensions, 3)
  expect_equal(bc3_excl$n_dimensions, 2)
  expect_equal(bc4_excl$n_dimensions, 3)
})

test_that("Test locations_df", {
  expect_equal(nrow(bc1$locations_df), bc1$n_available)
  expect_equal(nrow(bc2$locations_df), bc2$n_available)
  expect_equal(nrow(bc3_excl$locations_df), bc3_excl$n_available)
  expect_equal(nrow(bc4_excl$locations_df), bc4_excl$n_available)
})

context("Test BatchContainer sampel assignment")
samples <- data.frame(a=1:10, b=rnorm(10))

test_that("Test adding samples and then assigning them", {
  bc1_copy <- bc1$clone()
  bc1_copy$samples_df <- samples
  expect_null(bc1_copy$assignment_vec)
  assign_in_order(bc1_copy)
  expect_equal(bc1_copy$assignment_vec,
               c(seq_len(nrow(samples)), rep(NA_integer_, bc1_copy$n_available - nrow(samples))))

  bc1_copy <- bc1$clone()
  assign_in_order(bc1_copy, samples)
  expect_equal(bc1_copy$assignment_vec,
               c(seq_len(nrow(samples)), rep(NA_integer_, bc1_copy$n_available - nrow(samples))))
})


test_that("Test assigning samples randomly", {
  bc3_copy <- bc3_excl$clone()
  expect_null(bc3_copy$assignment_vec)
  expect_false(any(!is.na(bc3_copy$assignment_vec)))
  assign_random(bc3_copy, samples)
  expect_true(any(!is.na(bc3_copy$assignment_vec)))
})

test_that("Test assigning too many or empty samples", {
  bc3_copy <- bc3_excl$clone()
  bc4_copy <- bc4_excl$clone()
  expect_error(bc3_copy$samples_df <- data.frame())
  expect_error(bc3_copy$samples_df <- data.frame(a=seq_len(bc3_copy$n_available+1)))
  expect_error(bc4_copy$samples_df <- data.frame())
  expect_error(bc4_copy$samples_df <- data.frame(a=seq_len(bc4_copy$n_available+1)))
})

test_that("Test double sample assignment", {
  bc3_copy <- bc3_excl$clone()
  bc3_copy$samples_df <- samples
  expect_error(bc3_copy$samples_df <- samples)
  expect_error(assign_in_order(bc3_copy, samples))
  expect_error(assign_random(bc3_copy, samples))
  bc4_copy <- bc4_excl$clone()
  bc4_copy$samples_df <- samples
  expect_error(bc4_copy$samples_df <- samples)
  expect_error(assign_in_order(bc4_copy, samples))
  expect_error(assign_random(bc4_copy, samples))
})
