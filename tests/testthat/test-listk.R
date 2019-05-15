test_that("multiplication works", {
    a = 1
    b = 1:2
    c = 1:3
    l1 <- listk(a, b, c)
    l2 <- listk(a, b, c = 1:3)
    l3 <- listk(a = 1, b = 1:2, c = 1:3)
    l4 <- listk(1, 1:2, c)
    expect_equal(l1, l2)
    expect_equal(l1, l3)
    expect_equal(names(l4), c("1", "1:2", "c"))
})
