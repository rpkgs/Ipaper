#' slope_arr
#'
#' @param arr a matrix (`[ngrid, ntime]`) or array (`[nlon, nlat, ntime]`).
#' @param FUN slope functions, see [slope_mk()].
#' @param return.list boolean,
#' - `TRUE`: list(slope, pvalue) will be return
#' - `FALSE`: a array, with the dim of [nx, ny, 2].
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

# #' calculate MK slope of rast object
# #'
# #' @param r A rast object
# #' @param ... ignored
# #'
# #' @seealso [terra::rast()]
# #' @importFrom terra as.array plot rast ext
# #' @export
# slope_rast <- function(r, period = c(2001, 2020),
#     fun = rtrend::slope_mk,
#     outfile = NULL, overwrite = FALSE, ...)
# {
#     if (is.character(r)) r = rast(r, ...)
#     if (!is.null(period)) {
#         # `r` should have time information
#         year = year(time(r))
#         ind = which(year >= period[1] & year <= period[2])
#         arr = as.array(r[[ind]]) # 3d array
#     } else {
#         arr = as.array(r) # 3d array
#     }
#     t = slope_arr(arr, fun = fun, return.list = FALSE) # 3d array
#     r_target = rast(r, nlyrs = 2) %>%
#         set_names(c("slope", "pvalue"))
#         # vals = t,)# vals = ans,
#     values(r_target) = t # !note that `t` should be a 3d array
#     # `vals`, and `values`, result is different
#     if (!is.null(outfile)) {
#         if (!file.exists(outfile) || overwrite) {
#             # if (file.exists(outfile)) file.remove(outfile)
#             writeRaster(r_target, outfile, overwrite = TRUE)
#         }
#     }
#     r_target
# }

# slope_nc <- function(file, varname = 0) {
#     period = c("2001-01-01", "2020-12-31")
#     arr <- ncread(file, varname, DatePeriod = period)$data[[1]]
#     info = ncdim_get(file)

#     t = slope_arr(arr)
#     r = get_grid.lonlat(info$lon, info$lat)
#     r@data = t %>% list2df()
#     r
# }

# list2df <- function(x) {
#     lapply(x, as.numeric) %>% as.data.frame()
# }
