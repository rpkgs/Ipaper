test_that("slope works", {
    y <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    expect_equal(names(slope(cbind(y, y2 = y))), c("y", "y2"))
})
