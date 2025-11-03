#' slope_arr
#'
#' @param arr a matrix (`[ngrid, ntime]`) or array (`[nlon, nlat, ntime]`).
#' @param FUN slope functions, see [slope_mk()].
#' @param return.list boolean,
#' - `TRUE`: list(slope, pvalue) will be return
#' - `FALSE`: a array, with the dim of `[nx, ny, 2]`.
#'
#' @return t, A 3d array, with the dim of `[nx, ny, 2]`.
#' - `t[,,1]`: slope
#' - `t[,,2]`: pvalue
#' @export
slope_arr <- function(arr, FUN = rtrend::slope_mk, return.list = FALSE) {
  I_grid <- apply_3d(arr) %>% which.notna()
  mat <- array_3dTo2d(arr, I_grid)

  res <- apply_par(mat, 1, FUN)
  ans <- array_2dTo3d(res, I_grid, dim(arr)[1:2]) # trend

  if (return.list) {
    list(slope = ans[, , 1], pvalue = ans[, , 2])
  } else {
    ans
  }
}


# list2df <- function(x) {
#     lapply(x, as.numeric) %>% as.data.frame()
# }
