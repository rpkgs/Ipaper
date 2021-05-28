#' write_list2xlsx
#' 
#' @description base function, write list x into fileName with each list in workbook,
#' write shapefile dbf table into excel *.xlsx
#' 
#' @param x List object to be saved, `x` should have names and can't be NULL.
#' @param file xlsx file name
#' @param .progress name of the progress bar to use, see create_progress_bar.
#' @param rowNames a logical value indicating whether the row names are to 
#' be written.
#' 
#' @import openxlsx
#' @export
write_list2xlsx <- function(x, file, .progress = "none", rowNames = FALSE, show = TRUE)
{
    file <- file_name(file) %>% {paste0(dirname(file), "/", ., ".xlsx")}
    name <- deparse(substitute(x))
    if ("data.frame" %in% class(x)) {
        x <- setNames(list(x), name)
    }
    
    sheetNames <- names(x)
    if (is.null(sheetNames))
        sheetNames <- paste0("sheet", seq_along(x))
    wb <- createWorkbook()
    options(openxlsx.borderStyle = "none")
    hs1 <- createStyle(fgFill = "#DCE6F1", halign = "CENTER",
                       textDecoration = "Italic", border = "Bottom")
    writeIn <- function(i) {
        addWorksheet(wb, sheetNames[i])
        writeData(wb, sheetNames[i], x[[i]], colNames = TRUE, 
                  rowNames = rowNames, borders = "rows", headerStyle = hs1)
    }
    if (.progress != "none") cat(sprintf("[---- Writing into Workbook ----]\n"))
    tmp <- llply(seq_along(x), writeIn, .progress = .progress)
    if (.progress != "none") cat(sprintf("[---- Writing into xlsx file: %s ----]\n", file))
    saveWorkbook(wb, file, overwrite = TRUE)
    if (show) file.show(file)
}

#' read_xlsx2list
#' 
#' If excel file hava many sheets, this function also works.
#' 
#' @param file xlsx or xls file path
#' @param ... other parameters to [readxl::read_excel()]
#' 
#' @import openxlsx readxl
#' @importFrom plyr llply
#' @export
read_xlsx2list <- function(file, ...){
    cat(sprintf("[---- Reading File: %s ----]\n", file))
    ## judge whether it's xls or xlsx
    #  if file is *.xls use readxl::read_excel
    if (length(grep("xls$", basename(file))) != 0){
    sheetNames <- excel_sheets(file)
    x <- llply(sheetNames, function(sheet) as.data.table(read_excel(file, sheet, ...)),
               .progress = "text")
    }else{
        sheetNames <- getSheetNames(file)
        x <- llply(sheetNames, function(sheet) read.xlsx(file, sheet, ...),
           .progress = "text")
    }
    names(x) <- sheetNames
    x#quickly return
}

#' @export
fwrite2 <- function(x, file){
    write.table(x, file, sep = ",", row.names = FALSE, fileEncoding = "gbk")
}

#' read_xlsx
#' 
#' @importFrom openxlsx read.xlsx
#' @export
read_xlsx <- function(file, sheet = 1, ...){
    read.xlsx(file, sheet, ..., detectDates = TRUE) %>% data.table()
}
