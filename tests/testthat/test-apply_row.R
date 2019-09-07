test_that("apply_col and apply_row works", {
    mat <- matrix(rnorm(4*6), 4, 6)
    
    mat_bycol <- apply_col(mat, c(1, 1, 2, 2), colMeans)
    mat_byrow <- apply_row(mat, c(1, 1, 2, 2, 3, 3), rowMeans)
    
    expect_equal(dim(mat_bycol), c(2, 6))
    expect_equal(dim(mat_byrow), c(4, 3))
})
