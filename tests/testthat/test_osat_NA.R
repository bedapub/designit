N <- 100
df <- data.frame(
  a = sample(letters[1:2], N, replace = TRUE),
  b = sample(letters[3:5], N, replace = TRUE),
  c = NA,
  batch = sample(c("1", "2"), N, replace = TRUE)
)


test_that("no error when using batch scores with NA-columns", {
  expect_silent(osat_score(df, "batch", c("a", "b")))
})

test_that("warning when a feature has NA", {
  df_copy <- df
  df_copy$a[5] <- NA
  expect_warning(osat_score(df_copy, "batch", c("a", "b")))
})

test_that("warning when a batch has NAs", {
  df_copy <- df
  df_copy$batch[5] <- NA
  expect_warning(osat_score(df_copy, "batch", c("a", "b")))
})

test_that("error if a feature is all NAs", {
  df_copy <- df
  df_copy$a <- NA
  expect_error(osat_score(df_copy, "batch", c("a", "b")))
})

test_that("error if batch is all NAs", {
  df_copy <- df
  df_copy$batch <- NA
  expect_error(osat_score(df_copy, "batch", c("a", "b")))
})
