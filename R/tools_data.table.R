#' fread_dir
#' 
#' @param ... others to [data.table::fread()]
#' 
#' @importFrom data.table rbindlist data.table
#' @export
fread_dir <- function(indir, pattern = "*.csv", ..., .progress="text", list2df=FALSE) {
  fs = dir(indir, pattern, full.names = TRUE)
  fs = set_names(fs, gsub(".csv", "", basename(fs)))

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
fwrite2 <- function(x, file) {
  write.table(x, file, sep = ",", row.names = FALSE, fileEncoding = "gbk")
}
