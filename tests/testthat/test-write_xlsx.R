test_that("write_xlsx works", {
    d <- data.frame(x = 1:10) %>% data.table()
    l <- list(d, d)
    write_list2xlsx(l, "d.xlsx", show = FALSE)
    
    l2 <- read_xlsx2list("d.xlsx")
    expect_equal(l2[[1]], d)
    
    d2 <- read_xlsx("d.xlsx")
    expect_equal(d, d2)
    # expect_equal(2 * 2, 4)
})
