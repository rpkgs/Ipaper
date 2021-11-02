context("dt_ddply")

test_that("dt_ddply works", {
    dt = data.table(x = 1:10, y = 1:5)

    ans1 = dt_ddply(dt, .(y), function(d) { d[which.max(x), ] })
    ans2 = dt_ddply(dt, .(y), ~.[which.max(x)])
    expect_equal(ans1, ans2)
    expect_true(is.data.table(ans1))
    expect_true(is.data.table(ans2))
    expect_equal(ans1$x, 6:10)

    r2 = dt_ddply(dt %>% as.data.frame(), .(y), ~.x)
    expect_true(!is.data.table(r2))
})
