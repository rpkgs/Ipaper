## helpers ---------------------------------------------------------------------
make_arr <- function() {
  array(1:6,
    dim = c(2, 3),
    dimnames = list(site = c("A", "B"), date = c("d1", "d2", "d3"))
  )
}

make_arr3 <- function() {
  array(1:24,
    dim = c(2, 3, 4),
    dimnames = list(
      site = c("A", "B"),
      date = c("d1", "d2", "d3"),
      var  = paste0("v", 1:4)
    )
  )
}

## array2dt --------------------------------------------------------------------
test_that("array2dt: 维度列、行数与列名正确", {
  arr <- make_arr()
  dt <- array2dt(arr, dimnames(arr))

  expect_s3_class(dt, "data.table")
  expect_equal(nrow(dt), prod(dim(arr)))
  expect_equal(names(dt), c("site", "date", "value"))
})

test_that("array2dt: 展开顺序为最后一维变化最快 (CJ 顺序)", {
  arr <- make_arr()
  dt <- array2dt(arr, dimnames(arr))

  # 第一维 (site) 变化最慢，最后一维 (date) 变化最快
  expect_equal(dt$site, rep(c("A", "B"), each = 3))
  expect_equal(dt$date, rep(c("d1", "d2", "d3"), times = 2))
  # 值需与 array 下标严格对应
  expect_equal(dt[site == "A", value], c(1, 3, 5))
  expect_equal(dt[site == "B", value], c(2, 4, 6))
})

test_that("array2dt: 支持三维数组", {
  arr <- make_arr3()
  dt <- array2dt(arr, dimnames(arr))

  expect_equal(nrow(dt), 24)
  expect_equal(names(dt), c("site", "date", "var", "value"))
  expect_equal(dt[site == "A" & date == "d1", value], as.vector(arr["A", "d1", ]))
})

## dt2array --------------------------------------------------------------------
test_that("dt2array: 二维往返转换", {
  arr <- make_arr()
  expect_equal(dt2array(array2dt(arr, dimnames(arr))), arr)
})

test_that("dt2array: 三维往返转换", {
  arr <- make_arr3()
  expect_equal(dt2array(array2dt(arr, dimnames(arr))), arr)
})

test_that("dt2array: 值的填充不依赖 dt 的行顺序 (回归测试)", {
  arr <- make_arr3()
  dt <- array2dt(arr, dimnames(arr))

  # 维度 level 顺序恰好保持时, 应与原数组完全一致
  expect_equal(dt2array(dt[order(value)]), arr)
  expect_equal(dt2array(data.table::setkey(data.table::copy(dt), var)), arr)

  # 完全打乱行序: level 顺序按首次出现推断, 重排回原顺序后值应一致
  set.seed(1)
  r <- dt2array(dt[sample(.N)])
  r <- r[dimnames(arr)$site, dimnames(arr)$date, dimnames(arr)$var]
  expect_equal(r, arr)
})

test_that("dt2array: 缺失的维度组合记为 NA", {
  arr <- make_arr()
  dt <- array2dt(arr, dimnames(arr))
  dt_miss <- dt[!(site == "A" & date == "d2")]

  r <- dt2array(dt_miss)
  expect_equal(dim(r), dim(arr))           # 维度不缩减 (其余行仍含全部 level)
  expect_true(is.na(r["A", "d2"]))
  expect_equal(sum(is.na(r)), 1L)
})

test_that("dt2array: 保留值的数据类型", {
  # double
  arr_d <- array(c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
    dim = c(2, 3), dimnames = list(s = c("A", "B"), d = c("d1", "d2", "d3")))
  r_d <- dt2array(array2dt(arr_d, dimnames(arr_d)))
  expect_type(r_d, "double")
  expect_equal(r_d, arr_d)

  # character
  arr_c <- array(letters[1:6],
    dim = c(2, 3), dimnames = list(s = c("A", "B"), d = c("d1", "d2", "d3")))
  r_c <- dt2array(array2dt(arr_c, dimnames(arr_c)))
  expect_type(r_c, "character")
  expect_equal(r_c, arr_c)
})

test_that("dt2array: 支持非字符维度列 (走 match 分支)", {
  arr <- array(1:6, dim = c(2, 3),
    dimnames = list(id = c(10L, 20L), date = c("d1", "d2", "d3")))
  dt <- array2dt(arr, dimnames(arr))
  dt[, id := as.integer(id)]            # id 为 integer 列, 由 match 处理

  set.seed(1)
  r <- dt2array(dt[sample(.N)])
  expect_equal(r[as.character(c(10, 20)), dimnames(arr)$date], arr)
})

test_that("dt2array: 支持自定义值列名", {
  arr <- make_arr()
  dt <- array2dt(arr, dimnames(arr))
  data.table::setnames(dt, "value", "val")

  expect_equal(dt2array(dt, value_col = "val"), arr)
})
