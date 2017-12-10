#' @title writelist.xlsx
#' @description base function, write list x into fileName with each list in workbook,
#' write shapefile dbf table into excel *.xlsx
#' @details should be cautious that x attribute name can't be NULL should be
#' @examples
#' data(hydrodata)
#' writelist_ToXlsx("hydrodata.xlsx", hydrodata, .progress = "text")
#' @import openxlsx
#' @export
#'
writelist_ToXlsx <- function (x, fname, .progress = "text", rowNames = FALSE, ...)
{
  sheetNames <- names(x)
  if (is.null(sheetNames))
    sheetNames <- paste0("sheet", seq_along(x))
  wb <- createWorkbook()
  options(openxlsx.borderStyle = "none")
  hs1 <- createStyle(fgFill = "#DCE6F1", halign = "CENTER",
                     textDecoration = "Italic", border = "Bottom")
  writeIn <- function(i) {
    addWorksheet(wb, sheetNames[i])
    writeData(wb, sheetNames[i], x[[i]], colNames = TRUE, rowNames = rowNames, borders = "rows", headerStyle = hs1, ...)
  }
  if (.progress != "none")
    cat(sprintf("[---- Writing into Workbook ----]\n"))
  tmp <- llply(seq_along(x), writeIn, .progress = .progress)
  if (.progress != "none")
    cat(sprintf("[---- Writing into xlsx file: %s ----]\n",
                fname))
  saveWorkbook(wb, fname, overwrite = TRUE)
}

#' @title readxlsx_ToList
#' @description base function, if one excel file hava many sheets, this function will be work
#' @details should be cautious that x attribute name can't be NULL should be
#' @param fname file name included path going to be read.
#' @examples
#' fname <- system.file("data", "hydrodata.xlsx", package = "floodmap")
#' x <- readxlsx_ToList(fname)
#' @import openxlsx readxl
#' @importFrom plyr llply
#' @export
readxlsx_ToList <- function(fname, show = F, ...){
  cat(sprintf("[---- Reading File: %s ----]\n", fname))
  ## judge whether it's xls or xlsx
  #  if file is *.xls use readxl::read_excel
  if (length(grep("xls", basename(fname))) != 0){
    sheetNames <- excel_sheets(fname)
    x <- llply(sheetNames, function(sheet) as.data.frame(read_excel(fname, sheet, ...)),
               .progress = "text")
  }else{
    sheetNames <- getSheetNames(fname)
    x <- llply(sheetNames, function(sheet) read.xlsx(fname, sheet),
               .progress = "text")
  }
  names(x) <- sheetNames
  if (show) print(x)
  x#quickly return
}
