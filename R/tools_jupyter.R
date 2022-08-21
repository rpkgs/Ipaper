#' set_jupyter
#' 
#' @references 
#' 1. https://stackoverflow.com/questions/42729049/how-to-change-the-size-of-r-plots-in-jupyter
#' 
#' @export 
set_jupyter <- function(width = 10, height = 6, res = 200) {
  options(repr.plot.width = width, repr.plot.height = height, repr.plot.res = res)
}

#' @rdname set_jupyter
#' @export
set_dirRoot <- function(verbose = TRUE, dir = getwd()) {
  fs <- dir(dir, "*.Rproj")
  if (verbose) print(dir)
  if (length(fs) == 0) {
    set_dirRoot(verbose, dirname(dir))
  } else {
    setwd(dir)
  }
  invisible()
}
