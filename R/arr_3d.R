#' array_3dTo2d
#' 
#' @param array array with the dimension of `[nlon, nlat, ntime]`
#' @param I_grid subindex of `[nrow, ncol]`
#' 
#' @return
#' `[nlat*nlon, ntime]`
#' 
#' @export
array_3dTo2d <- function(array, I_grid){
    # array <- fliplr.3d(array)
    dim <- dim(array)
    if (length(dim) >= 3) {
        dim(array) <- c(prod(dim[1:2]), dim[3])
    }
    
    if (!missing(I_grid)) {
        array <-  array[I_grid, ]    
    }
    return(array)
}

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
#' arr <- array(rnorm(10*8*31), dim = size)
#' 
#' by <- c(rep(1, 10), rep(2, 21))
#' r2 <- apply_3d(arr, 3, by = by, FUN = rowMeans)
#' @importFrom matrixStats rowMeans2 rowMins rowMaxs
#' @export
apply_3d <- function(array, dim = 3, FUN = rowMeans2, by = NULL, ...) {
    # TODO: add by at here
    dims <- dim(array)
    ndim <- length(dims) # dimensions
    
    I_dims     <- setdiff(1:ndim, dim) # dimensions order
    dims_head  <- dims[I_dims]         # header dimensions 
    
    # move grouped dim to the last
    if (dim != ndim){
        array %<>% aperm(c(I_dims, dim)) 
    } 
    
    mat <- array_3dTo2d(array)

    if (is.null(by)) {
        ans <- FUN(mat, ..., na.rm = TRUE)    
        dim_new <- dims_head
    } else {
        dim_new <- c(dims_head, length(unique(by)))
        ans <- apply_row(mat, by, FUN)
    }
    dim(ans) <- dim_new
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
#' set_dim(x, c(3,4))
#' @export
set_dim <- function(x, dim){
    dim(x) <- dim
    x
}
