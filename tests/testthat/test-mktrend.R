test_that("multiplication works", {
    expect_silent({
      x <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
      r_cpp <- mkTrend_rcpp(x, IsPlot = FALSE)
    })
})
