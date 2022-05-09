bc1 <- BatchContainer$new(
  dimensions = c(row = 10, column = 10),
  exclude = data.frame(row = 3, column = c(2, 3))
)

test_that("Can assign from table", {
  bc2 <- BatchContainer$new(
    locations_table = bc1$get_locations()
  )
  expect_equal(bc1$get_locations(), bc2$get_locations())
})

test_that("Cannot assign from table with duplicated column names", {
  df <- bc1$get_locations()
  colnames(df) <- rep("a", ncol(df))
  expect_error({
    bc2 <- BatchContainer$new(
      locations_table = df
    )
  })
})
