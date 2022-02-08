#' write_sp2rgb
#' 
#' @param grid SpatialGridPixels or SpatialGridDataFrame object
#' @param brks 
#' @param cols 
#' 
#' @param mask Boolean vector indicate where to mask
#' @param col_mask
#' 
#' @examples
#' \dontrun{
#' write_sp2rgb(grid, brks, cols, file = "dem_pearl_rgb.tif")
#' }
#' @export
write_sp2rgb <- function(grid, brks, cols, file = "sp_rgb.tif", 
    mask = NULL, col_mask = "transparent") 
{
    col2dt <- function(cols) col2rgb(cols) %>% t() %>% as.data.table()
    colormap = col2dt(cols)

    ind   = findInterval(grid$band1, brks) # %>% summary()
    d_rgb = colormap[ind, ]
    
    grid_rgb   = grid
    grid_rgb@data <- d_rgb
    writeGDAL(grid_rgb, file, options = c("COMPRESS=DEFLATE")) # 
}

# if (!is.null(mask)) {
#     I_mask = which(mask)
#     d_rgb[I_mask, ] = 
# }
get_legend <- function(brks) {
    key = Ipaper::get_colorkey(brks, cols, is_factor = TRUE)
    g <- draw.colorkey(key)
    write_fig(g, "test/dem_legend.jpg", 9.2, 0.75)
}

#' get regular cdo grid
#' 
#' @param range `[lon_min, lon_max, lat_min, lat_max]`
#' @param cellsize double
#' @param outfile character, path of output file
#' 
#' @export 
cdo_grid <- function(range = c(70, 140, 15, 55), cellsize = 0.1, outfile = "grid.txt") {
    nlon = diff(range[1:2])/cellsize
    nlat = diff(range[3:4])/cellsize

    hcell = cellsize/2
    grid = glue("
    gridtype = lonlat
    xsize = {nlon}
    ysize = {nlat}
    xfirst = {range[1] + hcell}
    xinc = {cellsize}
    yfirst = {range[3] + hcell}
    yinc = {cellsize}
    ")
    writeLines(grid, outfile)
}
