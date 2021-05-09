test_that("apply_par works", {
    expect_equal(2 * 2, 4)
})

# library(rtrend)
# InitCluster(8)
# 
# nrow = 1e5; ncol = 41
# arr = matrix(rnorm(nrow*ncol), nrow, ncol)
# 
# system.time(r1 <- apply_par(arr, 1, rtrend::slope_p))
# system.time(r2 <- aaply(arr, 1, rtrend::slope_p))
