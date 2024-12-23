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
