test_that("bc$score() produces correct vector names", {
  bc <- BatchContainer$new(
    dimensions = c(row = 3, column = 3)
  )
  samp <- data.frame(sid = 1)
  bc <- assign_in_order(bc, samp)
  expect_equal(
    bc$score(
      list(
        a = function(...) c(1, 2),
        b = function(...) c(1),
        c = function(...) c(x=1, y=2),
        d = function(...) c(1)
      )
    ),
    setNames(
      c(1, 2, 1, 1, 2, 1),
      c("a1", "a2", "b", "cx", "cy", "d")
    )
  )
})
