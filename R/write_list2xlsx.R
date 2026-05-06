options(openxlsx.borderStyle = "none")
HEADING_STYLE <- createStyle(
  fgFill = "#DCE6F1",
  halign = "CENTER",
  textDecoration = "Italic",
  border = "Bottom"
)

#' write_sheet
#'
#' @description Write a single data frame into one sheet of a Workbook.
#' `wb` can be a `Workbook` object or a file path string. When a file path is
#' given, the workbook is loaded (or created if the file does not exist),
#' the sheet is written, and the file is saved back automatically.
#'
#' @param d A data frame.
#' @param file A file path string or an existing `Workbook` object.
#' @param sheetName Sheet name string.
#' @param overwrite if TRUE and `sheetName` already exists, replace it.
#'
#' @return The (modified) `Workbook` object, invisibly.
#'
#' @import openxlsx
#' @rdname write_sheets
#' @export
write_sheet <- function(d, file, sheetName, overwrite = FALSE) {
  sheetName <- as.character(sheetName)
  savefile <- NULL
  if (is.character(file)) {
    savefile <- file
    wb <- if (file.exists(savefile)) loadWorkbook(savefile) else createWorkbook()
    on.exit(rm(wb), add = TRUE)
  } else {
    wb <- file
  }

  pos <- if (overwrite && sheetName %in% names(wb)) {
    p <- which(names(wb) == sheetName)
    removeWorksheet(wb, sheetName)
    p
  } else {
    NULL
  }

  addWorksheet(wb, sheetName)
  writeData(wb, sheetName, d,
    colNames = TRUE, rowNames = FALSE,
    borders = "rows", headerStyle = HEADING_STYLE
  )

  if (!is.null(pos)) {
    n <- length(names(wb))
    worksheetOrder(wb) <- append(seq_len(n - 1L), n, after = pos - 1L)
  }

  if (!is.null(savefile)) saveWorkbook(wb, savefile, overwrite = TRUE)
  invisible(wb)
}


#' write_sheets
#'
#' @description Write a list of data frames into a Workbook, one sheet per
#' element. `wb` can be a `Workbook` object or a file path string. When a file
#' path is given, the workbook is loaded (or created if absent), written, and
#' saved back automatically. The file extension is normalised to `.xlsx`.
#'
#' @param lst List of data frames. Names are used as sheet names.
#' @param file A file path string or an existing `Workbook` object.
#' @param .progress name of the progress bar to use, see create_progress_bar.
#' @param show open the file after saving (only applies when `file` is a path).
#' @param overwrite_wb if TRUE (default) always create a fresh workbook when
#'   `file` is a path, even if the file already exists. Set to FALSE to
#'   load and append to an existing file.
#'
#' @return The (modified) `Workbook` object, invisibly.
#'
#' @import openxlsx
#' @export
write_sheets <- function(lst, file, .progress = "none", show = FALSE, overwrite_wb = TRUE) {
  name <- deparse(substitute(lst))
  if (inherits(lst, "data.frame")) {
    lst <- setNames(list(lst), name)
  }

  sheetNames <- names(lst)
  if (is.null(sheetNames)) {
    sheetNames <- paste0("sheet", seq_along(lst))
  }

  savefile <- NULL
  if (is.character(file)) {
    savefile <- paste0(dirname(file), "/", file_name(file), ".xlsx")
    wb <- if (!overwrite_wb && file.exists(savefile)) loadWorkbook(savefile) else createWorkbook()
    on.exit(rm(wb), add = TRUE)
  } else {
    wb <- file
  }

  if (.progress != "none") {
    cat("[---- Writing into Workbook ----]\n")
  }
  llply(seq_along(lst), function(i) {
    write_sheet(lst[[i]], wb, sheetNames[i])
  }, .progress = .progress)

  if (!is.null(savefile)) {
    if (.progress != "none") {
      cat(sprintf("[---- Writing into xlsx file: %s ----]\n", savefile))
    }
    saveWorkbook(wb, savefile, overwrite = TRUE)
    if (show) file_show(savefile)
  }
  invisible(wb)
}

#' @rdname write_sheets
#' @export
write_list2xlsx <- write_sheets


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
read_xlsx2list <- function(file, ...) {
  cat(sprintf("[---- Reading File: %s ----]\n", file))
  ## judge whether it's xls or xlsx
  #  if file is *.xls use readxl::read_excel
  if (length(grep("xls$", basename(file))) != 0) {
    sheetNames <- excel_sheets(file)
    lst <- llply(
      sheetNames,
      function(sheet) as.data.table(read_excel(file, sheet, ...)),
      .progress = "text"
    )
  } else {
    sheetNames <- getSheetNames(file)
    lst <- llply(sheetNames, function(sheet) read.xlsx(file, sheet, ...) %>% as.data.table(),
      .progress = "text"
    )
  }
  names(lst) <- sheetNames
  lst
}

#' read_xlsx
#'
#' @importFrom openxlsx read.xlsx
#' @export
read_xlsx <- function(file, sheet = 1, ...) {
  read.xlsx(file, sheet, ..., detectDates = TRUE) %>% as.data.table()
}
