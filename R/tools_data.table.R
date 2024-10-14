#' fread_dir
#' 
#' @param ... others to [data.table::fread()]
#' 
#' @importFrom data.table rbindlist data.table
#' @export
fread_dir <- function(indir, pattern = "*.csv", ..., .progress="text", list2df=TRUE) {
  fs = dir(indir, pattern, full.names = TRUE)
  fs = set_names(fs, basename(fs))

  res = llply(fs, fread, .progress=.progress, ...)
  if (!list2df) return(res)

  tryCatch({
    rbindlist(res)
  }, error = function(e) {
    message(sprintf('%s', e$message))
    res
  })
}

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
