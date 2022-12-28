test_that("main_dplyr works", {
  d1 = make_dt(
    pi, 2, 3, "5",
    4, 5, 6, "7", ncol = 4
  )
  d2 = tribble(
    ~V1, ~V2, ~V3, ~V4,
    pi, 2, 3, "5",
    4, 5, 6, "7"
  ) %>% as.data.table()
  
  expect_equal(d1, d2)
  expect_equal(dt_round(d1)$V1[1], 3.14)
  expect_equal(dt_chr2num(d1)$V4, c(5, 7))
})
