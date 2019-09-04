set.seed(1)
size <- c(10, 8, 31)
arr <- array(rnorm(10*8*31), dim = size)

test_that("apply_3d works", {
    # 1.1 no by
    expect_equal(dim(apply_3d(arr, 3)), c(10, 8))
    
    # 1.2 by
    by <- c(rep(1, 10), rep(2, 21))
    r2 <- apply_3d(arr, 3, by = by, FUN = rowMeans)
    means <- arr[, ,1:10] %>% set_dim(c(80, 10)) %>% rowMeans()
    
    expect_equal(dim(r2), c(10, 8, length(unique(by))))
    expect_true(all(r2[,,1] - means == 0))
    
    # 2 other dimension
    expect_equal(dim(apply_3d(arr, 1)), size[-1])
    expect_equal(dim(apply_3d(arr, 2)), size[-2])
})

test_that("apply_col works", {
    mat <- set_dim(arr, c(31, 80))    
    mat_byrow <- apply_row(mat, rep(letters[1:5], each = 16), rowMeans)
    expect_equal(dim(mat_byrow), c(31, 5))
})
