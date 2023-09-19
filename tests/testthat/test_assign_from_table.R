bc1 <- BatchContainer$new(
  dimensions = list(
    plate = 2,
    column = list(values = letters[1:3]),
    row = 3
  )
)

bc2 <- BatchContainer$new(
  dimensions = list(
    plate = 2,
    column = list(values = letters[1:3]),
    row = 3
  ),
  exclude = data.frame(plate = 2, row = 3, column = letters[1:3])
)

sample_sheet1 <- tibble::tribble(
  ~plate, ~column, ~row, ~sampleID,
  1, "a", 1, 1,
  1, "b", 2, 2,
  2, "a", 1, 3,
  2, "b", 2, 4,
  2, "a", 3, 5, # this line is excluded in bc2
)

sample_sheet2 <- head(sample_sheet1, -1)

# duplicate rows
sample_sheet3 <- rbind(
  sample_sheet1,
  sample_sheet1
)

sample_sheet4 <- rbind(
  sample_sheet1,
  data.frame(plate = 7, column = "a", row = 1, sampleID = 6)
)

test_that("assign samples from sample sheet", {
  bc <- bc1$copy()
  expect_null(bc$samples)
  bc <- assign_from_table(bc, sample_sheet1)
  expect_equal(nrow(sample_sheet1), nrow(bc$samples))

  bc <- bc1$copy()
  expect_null(bc$samples)
  bc <- assign_from_table(bc, sample_sheet2)
  expect_equal(nrow(sample_sheet2), nrow(bc$samples))

  bc <- bc2$copy()
  expect_null(bc$samples)
  bc <- assign_from_table(bc, sample_sheet2)
  expect_equal(nrow(sample_sheet2), nrow(bc$samples))
})

test_that("assign samples from sample sheet works when samples are added/assigned", {
  bc <- bc1$copy()
  expect_null(bc$samples)
  bc$samples <- subset(sample_sheet1, select = sampleID)
  bc <- assign_from_table(bc, sample_sheet1)

  bc <- bc2$copy()
  expect_null(bc$samples)
  bc <- assign_from_table(bc, sample_sheet2)
  bc <- assign_random(bc)
  bc <- assign_from_table(bc, sample_sheet2)
})


test_that("assign samples from sample sheet works only when samples match what's in the container", {
  bc <- bc1$copy()
  expect_null(bc$samples)
  bc <- assign_from_table(bc, sample_sheet1)
  expect_error(assign_from_table(bc, sample_sheet2))
})

test_that("assign into excluded locations should fail", {
  bc <- bc2$copy()
  expect_null(bc$samples)
  expect_error(assign_from_table(bc, sample_sheet1))
})

test_that("assign from table with duplicated rows should fail", {
  bc <- bc1$copy()
  expect_null(bc$samples)
  expect_error(assign_from_table(bc, sample_sheet3))
})

test_that("assign from table with duplicated rows should fail", {
  bc <- bc1$copy()
  expect_null(bc$samples)
  expect_error(assign_from_table(bc, sample_sheet3))
})
