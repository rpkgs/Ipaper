#' @title Ipaper
#' @name Ipaper
#' @aliases Ipaper-package
#' @docType package
#' @keywords download paper DOI
#' 
#' @importFrom stringr str_extract
#' @importFrom jsonlite fromJSON read_json
#' @importFrom purrr is_empty map transpose
#' @importFrom methods as
#' @importFrom data.table data.table
#' @importFrom graphics rect plot
#' @importFrom grid textGrob gpar grid.newpage
#' @importFrom grDevices cairo_pdf dev.off svg tiff
#' @importFrom stats quantile setNames
#' @importFrom utils object.size
#' @import httr xml2 magrittr plyr
#' 
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
# ' @useDynLib Ipaper .registration = TRUE
# ' @importFrom Rcpp sourceCpp
NULL
