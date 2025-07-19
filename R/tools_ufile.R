#' @export
read_ufile <- function(f, nodata = c(-99999, -9999)) {
  unit <- fread(f, nrows = 1)
  dat <- fread(f, skip = 2, header = FALSE)
  for (nd in nodata) {
    dat[dat == nd] <- NA
  }
  names(dat) <- names(unit)
  structure(list(data = dat, unit = unit), class = "unit_df")
}

# define a S3 method
#' @export
write_ufile <- function(x, ...) UseMethod("write_ufile")

#' @export
write_ufile.data.frame <- function(data, unit, fout) {
  fwrite(unit, fout)
  fwrite(data, fout, col.names = FALSE, append = TRUE)
}

#' @export
write_ufile.unit_df <- function(l, fout) {
  fwrite(l$unit, fout)
  fwrite(l$data, fout, col.names = FALSE, append = TRUE)
}

#' read csv or xls/xlsx file
#'
#' @param ... others to fun, one of [data.table::fread()], [readxl::read_xls()], [read_xlsx()]
#' @export
read_excel <- function(f, ...) {
  fun <- switch(file_ext(f),
    csv = data.table::fread,
    xls = readxl::read_xls,
    xlsx = readxl::read_xlsx
  )
  d <- fun(f, ...)
  if ("DOY" %in% names(d)) d <- dplyr::select(d, -DOY)
  d |> data.table()
}
