library(data.table)

test_that("melt_list works", {
    expect_silent({
        df <- data.frame(year = 2010, day = 1:2, month = 1, site = "A")
        l  <- list(a = df, b = df)
        melt_list(l, "id")
        
        l2 <- listk("type1" = l, "type2" = l)
        melt_tree(l2, c("type", "id"))
        
        # data.table
        df <- data.table(year = 2010, day = 1:2, month = 1, site = "A")
        l  <- list(a = df, b = df)
        melt_list(l, "id")
    })
})
