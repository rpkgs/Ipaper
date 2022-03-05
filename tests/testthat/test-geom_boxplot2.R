test_that("geom_boxplot2 works", {
    library(ggplot2)
    library(gridExtra)
    
    skip_on_appveyor()
    p <- ggplot(mpg, aes(class, hwy)) 
    p1 <- p + geom_boxplot() + 
        ggtitle("(a) geom_boxplot") + 
        theme(axis.text.x = element_blank(), 
              axis.title.x = element_blank(), 
              axis.ticks.x = element_blank())
    
    expect_silent(p2 <- p + geom_boxplot2(width.errorbar = 0.5) + 
                      ggtitle("(b) geom_boxplot2"))
    expect_equal(layer_scales(p2)$y$range$range, c(14, 36))
    
    g <- grid.arrange(p1, p2, heights = c(8, 9))
    # write_fig(g, "man/figure/geom_boxplot2.svg", 7, 5)
    
    p + geom_boxplot2()
    p + stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.5, color = "red") + 
        geom_boxplot2(show.errorbar = FALSE)
})
