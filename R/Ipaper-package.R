#' @title Ipaper
#' @name Ipaper
#' @aliases Ipaper-package
#' @docType package
#' @keywords paper
#'
#' @importFrom jsonlite fromJSON read_json
#' @importFrom purrr map transpose
#' @importFrom methods as
#' @importFrom data.table data.table
#' @importFrom graphics rect plot
#' @importFrom grid textGrob gpar grid.newpage
#' @importFrom grDevices cairo_pdf dev.off svg tiff colorRampPalette xy.coords
#' col2rgb jpeg
#' @importFrom stats quantile setNames end start
#' @importFrom utils object.size
#' @importFrom graphics abline grid legend par
#' @importFrom stats acf as.formula lm median na.omit pnorm qnorm density
#' @importFrom utils write.table modifyList str
#' @importFrom plyr llply ddply aaply
#'
#' @import magrittr
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  options("datatable.print.class" = TRUE)
  invisible()
}


file_info <- function(..., extra_cols = TRUE) {
  res <- suppressWarnings(.Internal(file.info(fn <- c(...), extra_cols)))
  res$mtime <- .POSIXct(res$mtime)
  res$ctime <- .POSIXct(res$ctime)
  res$atime <- .POSIXct(res$atime)
  class(res) <- "data.frame"
  attr(res, "row.names") <- fn
  res
}
