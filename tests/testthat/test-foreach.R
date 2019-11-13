context("test-foreach")
library(iterators)

test_that("numeric vector works", {
    x <- setNames(1:4, 1:4)
    y <- foreach(i = x) %do% { i }

    expect_equal(names(x), names(y))
})

test_that("list  works", {
    x <- as.list(setNames(1:4, 1:4))
    y <- foreach(i = x) %do% { i }

    expect_equal(names(x), names(y))
})

test_that("list  works", {
    x <- data.frame(1, 2, 3, 4)
    y <- foreach(i = x) %do% { i }

    expect_equal(names(x), names(y))
})

# context("test-rownames")
test_that("NULL rownames works", {
    x <- c("A", "B", "C", "D", "E")
    y <- seq(1:5)
    xy <- data.frame(x,y)
    r <- foreach(i=unique(x), .combine=rbind) %do% {
        c("First"=as.character(i), "Second"=sum(y<3 & x==i))
    }
    expect_null(rownames(r))
})

test_that("matrix works", {
    mat <- matrix(rnorm(1000), 100, 10)
    mat1 <- foreach(y = iter(mat, "row"), .combine = rbind) %do%{ y }
    mat2 <- foreach(y = iter(mat, "row"), .combine = rbind) %do%{ y[1, ] }

    mat_11 <- as.matrix(mat1)

    l1 <- list(mat1)
    l2 <- list(mat2)

    expect_equal(mat, mat1)
    # expect_equal(mat, mat2)
})
