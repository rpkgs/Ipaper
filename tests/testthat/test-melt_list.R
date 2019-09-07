test_that("melt_list works", {
    library(data.table)
    # data.frame
    df <- data.frame(year = 2010, day = 1:3, month = 1, variable = "A")
    l  <- list(a = df, b = df, c = NULL)
    expect_silent(df1 <- melt_list(l, "id"))

    # data.table
    dt <- data.table::data.table(year = 2010, day = 1:3, month = 1, site = "A")
    df2 <- melt_list(list(a = dt, b = dt))
    expect_true(is.data.table(df2))
    
    expect_error({
        l  <- list(a = dt, b = df)
        df2 <- melt_list(l, "id")
    })
})

test_that("melt_tree works", {
    d <- data.frame(year = 2010, day = 1:3, month = 1, variable = "A")
    l <- list(list(x = d))
    df <- melt_tree(l, c("a", "b"))
    expect_equal(colnames(df)[1:2], c("a", "b"))
})
