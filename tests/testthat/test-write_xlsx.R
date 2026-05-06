test_that("write_xlsx works", {
    d  <- data.frame(x = 1:10) %>% data.table()
    l  <- list(d, d)
    # f  <- tempfile(fileext = ".xlsx")
    f  <- "d.xlsx"
    write_list2xlsx(l, f, show = FALSE)

    l2 <- read_xlsx2list(f)
    expect_equal(l2[[1]], d)

    d2 <- read_xlsx(f)
    expect_equal(d, d2)

    d2 <- data.frame(x = 1:20) %>% data.table()
    write_sheet(d2, f, "sheet2", overwrite = TRUE)
    file.remove(f)
})

test_that("write_sheets creates new file with correct sheets", {
    f <- tempfile(fileext = ".xlsx")
    write_sheets(list(A = iris, B = mtcars), file = f)
    expect_equal(getSheetNames(f), c("A", "B"))
})

test_that("write_sheet appends sheet to existing file", {
    f <- tempfile(fileext = ".xlsx")
    write_sheets(list(A = iris, B = mtcars), file = f)
    write_sheet(airquality, f, "C")               # write_sheet always appends
    expect_equal(getSheetNames(f), c("A", "B", "C"))
})

test_that("write_sheets appends when overwrite_wb = FALSE", {
    f <- tempfile(fileext = ".xlsx")
    write_sheets(list(A = iris), file = f)
    write_sheets(list(B = mtcars), file = f, overwrite_wb = FALSE)
    expect_equal(getSheetNames(f), c("A", "B"))
})

test_that("write_sheet overwrite preserves sheet order and data", {
    f <- tempfile(fileext = ".xlsx")
    write_sheets(list(A = iris, B = mtcars, C = airquality), file = f)
    write_sheet(head(iris, 3), f, "B", overwrite = TRUE)
    expect_equal(getSheetNames(f), c("A", "B", "C"))
    expect_equal(nrow(read.xlsx(f, sheet = "B")), 3L)
})

test_that("write_sheets works with Workbook object", {
    wb <- createWorkbook()
    write_sheets(list(X = iris), file = wb)
    expect_true("X" %in% names(wb))
})

test_that("write_list2xlsx is an alias for write_sheets", {
    expect_identical(write_list2xlsx, write_sheets)
})

test_that("write_sheet handles numeric sheetName without index ambiguity", {
    wb <- createWorkbook()
    addWorksheet(wb, "A")          # occupies index 1
    write_sheet(iris, wb, 1L)      # should write to sheet named "1", not index 1
    expect_equal(names(wb), c("A", "1"))
    expect_equal(nrow(read.xlsx(wb, sheet = "1")), nrow(iris))
    expect_null(suppressWarnings(read.xlsx(wb, sheet = "A")))
})
