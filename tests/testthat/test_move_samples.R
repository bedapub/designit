bc <- BatchContainer$new(
  dimensions = c(row = 3, column = 3)
)
samp <- data.frame(i = 1:9)
bc <- assign_in_order(bc, samp)

test_that("assigning $assignment generates a warning", {
  expect_silent(bc$assignment)
  expect_warning(bc$assignment <- rev(1:9))
  expect_equal(bc$assignment, rev(1:9))
})

test_that("providing src & location_assignment (or none of the above) generates an error", {
  expect_silent(bc$move_samples(src = c(1L, 2L), dst = c(2L, 1L)))
  expect_silent(bc$move_samples(location_assignment = 1:9))
  expect_error(bc$move_samples(src = c(1L, 2L), dst = c(2L, 1L), location_assignment = 1:9))
  expect_error(bc$move_samples(dst = c(2L, 1L)))
  expect_error(bc$move_samples())
})


bc <- BatchContainer$new(
  dimensions = c(row = 3, column = 3)
)
samp <- data.frame(i = 1:8)
bc <- assign_in_order(bc, samp)

test_that("$move_samples() works as expected with src & dst (without $get_samples())", {
  bc$move_samples(src = c(1L, 2L), dst = c(2L, 1L))
  bc$move_samples(src = c(2L, 3L), dst = c(3L, 2L))
  bc$move_samples(src = c(3L, 9L), dst = c(9L, 3L))
  expect_equal(
    bc$get_samples(include_id = TRUE, as_tibble = FALSE)$.sample_id,
    c(2, 3, NA, 4, 5, 6, 7, 8, 1)
  )
})

test_that("$move_samples() accepts integers without L prexif", {
  expect_silent(bc$move_samples(src = c(1, 2), dst = c(2, 1)))
  expect_silent(bc$move_samples(location_assignment = as.double(c(rev(1:8), NA))))
})

test_that("$move_samples() does not accept non-integers", {
  expect_error(bc$move_samples(src = c(1.1, 2), dst = c(2, 1)))
  expect_error(bc$move_samples(src = c(1, 2), dst = c(2, 1.1)))
  expect_error(bc$move_samples(src = c(Inf, 2), dst = c(2, 1)))
  expect_error(bc$move_samples(src = c(1, 2), dst = c(2, Inf)))
  expect_error(bc$move_samples(src = c(NA, 2), dst = c(2, 1)))
  expect_error(bc$move_samples(src = c(1, 2), dst = c(2, NA)))
  expect_error(bc$move_samples(location_assignment = c(rev(1:7), 8.1, NA)))
  expect_error(bc$move_samples(location_assignment = c(rev(1:7), 8, Inf)))
})


bc <- assign_in_order(bc)

test_that("$move_samples() works as expected with src & dst (after $get_samples())", {
  bc$get_samples()
  bc$move_samples(src = c(1L, 2L), dst = c(2L, 1L))
  bc$move_samples(src = c(2L, 3L), dst = c(3L, 2L))
  bc$move_samples(src = c(3L, 9L), dst = c(9L, 3L))
  expect_equal(
    bc$get_samples(include_id = TRUE, as_tibble = FALSE)$.sample_id,
    c(2, 3, NA, 4, 5, 6, 7, 8, 1)
  )
})

test_that("$move_samples() generates errors when src & dst are different or zero length", {
  expect_error(bc$move_samples(src = 1L, dst = c(2L, 1L)))
  expect_error(bc$move_samples(src = 1L, dst = integer()))
  expect_error(bc$move_samples(src = integer(), dst = integer()))
  expect_error(bc$move_samples(src = 1L))
  expect_error(bc$move_samples(dst = c(2L, 1L)))
})

test_that("$move_samples() generates errors when location_assignment is incorrect", {
  expect_silent(bc$move_samples(location_assignment = as.integer(c(1:8, NA))))
  expect_error(bc$move_samples(location_assignment = 1:8))
})

bc <- BatchContainer$new(
  dimensions = c(row = 3, column = 3)
)
samp <- data.frame(i = 1:8)
bc <- assign_in_order(bc, samp)

test_that("$move_samples() works as expected with location_assignment (without $get_samples())", {
  a <- as.integer(c(2, 3, NA, 4, 5, 6, 7, 8, 1))
  b <- c(1:8, NA_integer_)
  bc$move_samples(location_assignment = a)
  expect_equal(
    bc$get_samples(include_id = TRUE, as_tibble = FALSE)$.sample_id,
    a
  )
  bc$move_samples(location_assignment = b)
  expect_equal(
    bc$get_samples(include_id = TRUE, as_tibble = FALSE)$.sample_id,
    b
  )
})

test_that("$move_samples() works as expected with location_assignment (after $get_samples())", {
  bc$get_samples(as_tibble = FALSE)
  a <- as.integer(c(2, 3, NA, 4, 5, 6, 7, 8, 1))
  b <- c(1:8, NA_integer_)
  bc$move_samples(location_assignment = a)
  expect_equal(
    bc$get_samples(include_id = TRUE, as_tibble = FALSE)$.sample_id,
    a
  )
  bc$move_samples(location_assignment = b)
  expect_equal(
    bc$get_samples(include_id = TRUE, as_tibble = FALSE)$.sample_id,
    b
  )
})
