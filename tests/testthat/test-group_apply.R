test_that("upper_envelope works", {
    y = rnorm(1000)
    x = seq_along(y)
    
    
    expect_true({
        r = group_upperEnvelope(x, y, 20)
        plot(x, y)
        lines(y~x, r, col = "red")
        is.data.table(r) && nrow(r) == 49
    })
    
    expect_true({
        r2 = group_apply(x, y, chunk = 20)
        is.data.table(r2) && nrow(r2) == 49
    })
})
