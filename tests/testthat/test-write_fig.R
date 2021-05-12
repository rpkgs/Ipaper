test_that("write_fig works", {
    library(ggplot2)
    p <- ggplot(mpg, aes(class, hwy))
    p1 <- p + geom_boxplot()
    
    expect_true({
        write_fig(p1, "a.svg", show = FALSE)
        write_fig(p1, "a.pdf", show = FALSE)
        write_fig(p1, "a.tif", show = FALSE)
        TRUE    
    })
})
