# #' flipud and fliplr
# #' @export
# flipud <- function(x, ...) UseMethod("flipud", x)

#' flipud and fliplr
#' @export
flipud <- function(x, ...) {
  I <- ncol(x):1
  ndim <- length(dim(x))
  if (ndim == 2) {
    x[, I]
  } else if (ndim == 3) {
    x[, I, ]
  }
}

#' @export
#' @rdname flipud
fliplr <- function(x) {
  I <- nrow(x):1
  ndim <- length(dim(x))
  if (ndim == 2) {
    x[I, ]
  } else if (ndim == 3) {
    x[I, , ]
  }
  # x
}
