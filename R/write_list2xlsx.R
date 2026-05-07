HEADING_FILL_COLOR <- "#DCE6F1"

#' write_sheet
#'
#' @description Write a single data frame into one sheet of a Workbook.
#' `file` can be a `wbWorkbook` object or a file path string. When a file path
#' is given, the workbook is loaded (or created if the file does not exist),
#' the sheet is written, and the file is saved back automatically.
#'
#' @param d A data frame.
#' @param file A file path string or an existing `wbWorkbook` object.
#' @param sheetName Sheet name string.
#' @param overwrite if TRUE and `sheetName` already exists, replace it.
#'
#' @return The (modified) `wbWorkbook` object, invisibly.
#'
#' @import openxlsx2
#' @rdname write_sheets
#' @export
write_sheet <- function(d, file, sheetName, overwrite = FALSE) {
  sheetName <- as.character(sheetName)
  savefile <- NULL
  if (is.character(file)) {
    savefile <- file
    wb <- if (file.exists(savefile)) wb_load(savefile) else wb_workbook()
    on.exit(rm(wb), add = TRUE)
  } else {
    wb <- file
  }

  sheet_names <- wb$get_sheet_names()
  pos <- if (overwrite && sheetName %in% sheet_names) {
    p <- which(sheet_names == sheetName)
    wb$remove_worksheet(sheetName)
    p
  } else {
    NULL
  }

  wb$add_worksheet(sheetName)
  wb$add_data(sheetName, d, col_names = TRUE, row_names = FALSE)

  nc <- ncol(d)
  if (nc > 0L) {
    dims <- wb_dims(rows = 1, cols = seq_len(nc))
    wb$add_fill(sheetName, dims = dims, color = wb_color(hex = HEADING_FILL_COLOR))
    wb$add_font(sheetName, dims = dims, italic = "1")
    wb$add_border(sheetName, dims = dims,
                  bottom_border = "thin", bottom_color = wb_color(hex = "000000"))
  }

  if (!is.null(pos)) {
    all_names <- wb$get_sheet_names()
    n <- length(all_names)
    new_idx <- append(seq_len(n - 1L), n, after = pos - 1L)
    wb$set_order(all_names[new_idx])
  }

  if (!is.null(savefile)) wb$save(savefile, overwrite = TRUE)
  invisible(wb)
}


#' write_sheets
#'
#' @description Write a list of data frames into a Workbook, one sheet per
#' element. `file` can be a `wbWorkbook` object or a file path string. When a
#' file path is given, the workbook is loaded (or created if absent), written,
#' and saved back automatically. The file extension is normalised to `.xlsx`.
#'
#' @param lst List of data frames. Names are used as sheet names.
#' @param file A file path string or an existing `wbWorkbook` object.
#' @param .progress name of the progress bar to use, see create_progress_bar.
#' @param show open the file after saving (only applies when `file` is a path).
#' @param overwrite_wb if TRUE (default) always create a fresh workbook when
#'   `file` is a path, even if the file already exists. Set to FALSE to
#'   load and append to an existing file.
#'
#' @return The (modified) `wbWorkbook` object, invisibly.
#'
#' @import openxlsx2
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
    wb <- if (!overwrite_wb && file.exists(savefile)) wb_load(savefile) else wb_workbook()
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
    wb$save(savefile, overwrite = TRUE)
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
#' @import openxlsx2
#' @importFrom plyr llply
#' @export
read_xlsx2list <- function(file, ...) {
  cat(sprintf("[---- Reading File: %s ----]\n", file))
  if (length(grep("xls$", basename(file))) != 0) {
    sheetNames <- excel_sheets(file)
    lst <- llply(
      sheetNames,
      function(sheet) as.data.table(read_excel(file, sheet, ...)),
      .progress = "text"
    )
  } else {
    wb <- wb_load(file)
    sheetNames <- unname(wb$get_sheet_names())
    lst <- llply(sheetNames, function(sheet) wb$to_df(sheet = sheet) %>% as.data.table(),
      .progress = "text"
    )
  }
  names(lst) <- sheetNames
  lst
}

#' read_xlsx
#'
#' @importFrom openxlsx2 wb_read
#' @export
read_xlsx <- function(file, sheet = 1, ...) {
  wb_read(file, sheet = sheet, ...) %>% as.data.table()
}
