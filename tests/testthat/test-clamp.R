test_that("clamp works", {
    x = 1:10
    
    r = clamp(x, c(2, 8))
    expect_equal(min(r), 2)
    expect_equal(max(r), 8)
    
    # 
    r = clamp(x, c(2, 8), fill.na = TRUE)
    expect_equal(which.notna(r), 2:8)
    expect_equal(which.na(r), c(1, 9, 10))
})
