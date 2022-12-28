test_that("tools works", {
  # cut_levels
  x <- c(-0.09, -0.4, 0.04, 0.15)
  lev <- cut_plevels(x, verbose = TRUE)
  expect_equal(length(levels(lev)), 8)

  # runningId
  expect_no_error({
    for (i in 1:10) runningId(i, prefix = "Ipaper", 3)
  })

  # print2
  expect_no_error({print2(x, lev)})

  # match2
  info = match2(x = c(1, 2, 4), y = c(1, 3, 2, 4))
  expect_equal(info$I_x, c(1L, 2L, 3L))
  expect_equal(info$I_y, c(1L, 3L, 4L))

  # unique_sort
  expect_equal(unique_sort(c(2, 2, 3, NA)), c(2, 3, NA))

  # unique_length
  expect_equal(unique_length(c(2, 2, 3, NA)), 3)
})
