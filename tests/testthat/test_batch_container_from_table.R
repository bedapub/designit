test_that(
  "Passing invalid inputs to batch_container_from_table causes an error",
  {
    # empty input
    expect_error(batch_container_from_table())
    # missing inputs
    expect_error(batch_container_from_table(1:3))
    expect_error(batch_container_from_table(1:3, 1:3))
    # empty table
    expect_error(batch_container_from_table(data.frame(), 1:3))
    # missing columns
    expect_error(batch_container_from_table(data.frame(x = 2), "y"))
    # no sample columns
    expect_error(batch_container_from_table(data.frame(x = 2), "x"))
  }
)

test_that(
  "Passing incomplete table to batch_container_from_table causes an error",
  {
    expect_error(batch_container_from_table(data.table(x = 2:4, y = NA), "x"))
  }
)

test_that(
  "Basic batch_container_from_table usage",
  {
    tab <- data.frame(
      row = rep(1:3, each = 3),
      column = rep(1:3, 3),
      sample_id = 1:9
    )
    bc <- batch_container_from_table(tab, c("row", "column"))
    expect_true(all_equal_df(bc$get_samples(), tab))
  }
)

test_that(
  "batch_container_from_table usage with NAs",
  {
    tab <- data.frame(
      row = rep(1:3, each = 3),
      column = rep(1:3, 3),
      sample_id = c(1, 2, 3, NA, 5, 6, 7, NA, 9)
    )
    bc <- batch_container_from_table(tab, c("row", "column"))
    expect_true(all_equal_df(bc$get_samples(), tab))
  }
)

test_that(
  "batch_container_from_table with all-NA locations",
  {
    tab <- data.frame(
      row = rep(1:3, each = 3),
      column = rep(1:3, 3),
      sample_id = c(1, 2, 3, NA, 5, 6, 7, NA, 9)
    )
    tab[1, 1] <- NA
    tab[1, 2] <- NA
    expect_warning({
      bc <- batch_container_from_table(tab, c("row", "column"))
    })
    expect_true(all_equal_df(bc$get_samples(), tab))
  }
)

test_that(
  "batch_container_from_table with all-NA samples should cause an error",
  {
    tab <- data.frame(
      row = rep(1:3, each = 3),
      column = rep(1:3, 3),
      sample_id = NA
    )
    expect_error({
      bc <- batch_container_from_table(tab, c("row", "column"))
    })
  }
)
