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
#' 
#' @note This function also suits for big.matrix object.
#'
#' @examples
#' mat <- matrix(rnorm(4*6), 4, 6)
#' mat_bycol <- apply_col(mat, c(1, 1, 2, 2), colMeans)
#' mat_byrow <- apply_row(mat, c(1, 1, 2, 2, 3, 3), rowMeans)
#' 
#' @importFrom matrixStats colMeans2 rowMeans2 colMins colMaxs rowMins rowMaxs 
#' @export
apply_col <- function(mat, by, FUN = colMeans2, ...) {
    if (length(by) != nrow(mat)) {
        stop('Length of by is not equal to nrow of mat')
    }
    grps <- unique(by) %>% sort()

    foreach(grp = grps, .combine = rbind) %do% {
        I <- which(by == grp)
        FUN(mat[I, ], na.rm = TRUE)
    } %>% set_rownames(grps) %>% 
    set_colnames(colnames(mat))
}


#' @rdname apply_col
#' @export
apply_row <- function(mat, by, FUN = rowMeans2, ...) {
    if (length(by) != ncol(mat)) {
        stop('Length of by is not equal to ncol of mat')
    }
    grps <- unique(by) %>% sort()

    foreach(grp = grps, .combine = cbind) %do% {
        I <- which(by == grp)
        FUN(mat[, I], na.rm = TRUE, ...)
    } %>% set_colnames(grps) %>% 
    set_rownames(rownames(mat))
}
