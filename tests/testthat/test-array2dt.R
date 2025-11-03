test_that("array2dt works", {
  # 测试往返转换
  arr <- array(1:6,
    dim = c(2, 3),
    dimnames = list(
      site = c("A", "B"),
      date = c("d1", "d2", "d3")
      # var = c("v1", "v2", "v3", "v4")
    )
  )

  # array -> dt
  dt <- array2dt(arr, dimnames(arr))
  expect_equal(dt[site == "A", value], c(1, 3, 5))

  # dt -> array
  arr2 <- dt2array(dt)
  expect_equal(arr, arr2)
})
