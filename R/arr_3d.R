#' apply function for 3d array
#'
#' NA values will be removed automatically
#'
#' @param array A 3d array
#' @param dim giving the subscripts to split up data by.
#' @param FUN function, should only be row applied function, e.g. `matrixStats::rowMeans2`,
#' `matrixStats::rowMins`, `matrixStats::rowRanges`.
#' Because 3d array will be convert to matrix first, with the aggregated dim in
#' the last dimension.
#' @param by
#' * If not provided (`NULL`), the aggregated dim will be disappear.
#'   For example, daily precipitation `[nrow, ncol, 31-days]` aggregate into
#'   monthly `[nrow, ncol]`.
#' * If provided, `by` should be equal to the aggregated `dim`.
#'   For example, daily precipitation `[nrow, ncol, 365-days]` aggregate into
#'   monthly `[nrow, ncol, 12-months]`. In that situation, `by` should be equal
#'   to 365, and be `format(date, '%Y%m')`.
#'
#' @inheritParams set_dim
#' @inheritParams apply_row
#'
#' @seealso [apply_row] [matrixStats::rowRanges]
#'
#' @examples
#' set.seed(1)
#' size <- c(10, 8, 31)
#' arr <- array(rnorm(10 * 8 * 31), dim = size)
#'
#' by <- c(rep(1, 10), rep(2, 21))
#' r2 <- apply_3d(arr, 3, by = by, FUN = rowMeans)
#'
#' \dontrun{
#' arr_yearly <- apply_3d(arr, by = year(dates), scale = days_in_month(dates))
#' }
#' @importFrom matrixStats rowMeans2 rowMins rowMaxs
#' @export
apply_3d <- function(array, dim = 3, FUN = rowMeans2, by = NULL, scale = 1, na.rm = TRUE, ...) {
  # TODO: add by at here
  dims <- dim(array)
  ndim <- length(dims) # dimensions

  I_dims <- setdiff(1:ndim, dim) # dimensions order
  dims_head <- dims[I_dims] # header dimensions

  # move grouped dim to the last
  if (dim != ndim) {
    array %<>% aperm(c(I_dims, dim))
  }

  mat <- array_3dTo2d(array)

  if (is.null(by)) {
    ans <- FUN(mat, ..., na.rm = na.rm)
    dim_new <- dims_head
  } else {
    dim_new <- c(dims_head, length(unique(by)))
    ans <- apply_row(mat, by, FUN, scale = scale)
  }
  dim(ans) <- dim_new
  ans
}

#' apply_col
#'
#' * `apply_col`: aggregate by col, return a `[ngrp, ncol]` matrix
#' * `apply_row`: aggregate by row, return a `[nrow, ngrp]` matrix
#'
#' For example, setting the dimension of `mat` is `[ngrid, ntime]`,
#' if you want to aggregate by time, `apply_row` should be used here;
#' if you want to aggregate by region (grids), `apply_col` should be used.
#'
#' @param mat matrix, `[nrow, ncol]`
#' @param by integer vector, with the dim of `[ntime]`
#' @param scale in the same length of `by`, or a const value,
#' `value_returned` = `FUN(x)*scale`. This parameter is designed for converting
#' monthly to yearly, meanwhile multiply days in month.
#' Currently, same group should have the same scale factor. Otherwise, only the
#' first is used.
#' @note This function also suits for big.matrix object.
#'
#' @examples
#' mat <- matrix(rnorm(4 * 6), 4, 6)
#' mat_bycol <- apply_col(mat, c(1, 1, 2, 2), colMeans)
#' mat_byrow <- apply_row(mat, c(1, 1, 2, 2, 3, 3), rowMeans)
#'
#' @importFrom matrixStats colMeans2 rowMeans2 colMins colMaxs rowMins rowMaxs
#' @export
apply_col <- function(mat, by, FUN = colMeans2, scale = 1, ...) {
  if (length(by) != nrow(mat)) {
    stop("Length of by is not equal to nrow of mat")
  }
  if (length(scale) == 1) scale <- rep(scale, length(by))
  grps <- unique(by) %>% sort()

  ans <- lapply(grps, function(grp) {
    I <- which(by == grp)
    factor <- scale[I][1]
    FUN(mat[I, , drop = FALSE] * factor, na.rm = TRUE, ...)
  }) %>% do.call(rbind, .)

  if (!is.matrix(ans)) ans <- as.matrix(ans)
  ans %>%
    set_rownames(grps) %>%
    set_colnames(colnames(mat))
}


#' @rdname apply_col
#' @export
apply_row <- function(mat, by, FUN = rowMeans2, scale = 1, ...) {
  if (length(by) != ncol(mat)) {
    stop("Length of by is not equal to ncol of mat")
  }
  if (length(scale) == 1) scale <- rep(scale, length(by))
  grps <- unique(by) %>% sort()

  ans <- lapply(grps, function(grp) {
    I <- which(by == grp)
    factor <- scale[I][1]
    FUN(mat[, I, drop = FALSE] * factor, na.rm = TRUE, ...)
  }) %>% do.call(cbind, .)

  if (!is.matrix(ans)) ans <- as.matrix(ans)
  ans %>%
    set_colnames(grps) %>%
    set_rownames(rownames(mat))
}

#' array_3dTo2d
#'
#' @param array array with the dimension of `[nlon, nlat, ntime]`
#' @param I_grid subindex of `[nrow, ncol]`
#'
#' @return
#' `[nlat*nlon, ntime]`
#'
#' @export
array_3dTo2d <- function(array, I_grid = NULL) {
  dim <- dim(array)
  names_last <- dimnames(array) %>% last()

  if (length(dim) >= 3) {
    dim(array) <- c(prod(dim[1:2]), dim[3])
  }
  if (!is.null(I_grid)) {
    array <- array[I_grid, ]
  }
  if (!is.null(names_last)) array %<>% set_dimnames(list(NULL, names_last))
  return(array)
}

#' @param dim `[nrow, ncol]`
#'
#' @rdname array_3dTo2d
#' @export
array_2dTo3d <- function(array, I_grid = NULL, dim) {
  ntime <- dim(array) %>% last()
  dim <- c(dim, ntime)

  temp <- array(NA * array[1], dim = dim) %>%
    array_3dTo2d()
  if (is.null(I_grid)) {
    temp <- array
  } else {
    temp[I_grid, ] <- array
  }
  ans <- set_dim(temp, dim)
  names_last <- dimnames(array) %>% last()
  if (!is.null(names_last)) ans %<>% set_dimnames(list(NULL, NULL, names_last))
  ans
}

#' Set dimensions of an Object
#'
#' @inheritParams base::dim
#'
#' @seealso [base::dim]
#'
#' @examples
#' x <- 1:12
#' set_dim(x, c(3, 4))
#' @export
set_dim <- function(x, dim) {
  dim(x) <- dim
  x
}

#' @export
set_dimnames <- function(x, value) {
  dimnames(x) <- value
  x
}
