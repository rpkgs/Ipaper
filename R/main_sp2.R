
raster2poly <- function(r, I_grid = NULL){
    if (is.null(I_grid)) I_grid <- 1:nrow(r)
    as(r, "SpatialPolygonsDataFrame")
}

raster2SpatialGrid <- function(r, I_grid = NULL){
    if (is.null(I_grid)) I_grid <- 1:nrow(r)
    as(r, "SpatialGridDataFrame")
}


#' Calculate area of spatial object
#' 
#' @param grid SpatialPolygonsDataFrame or SpatialGridDataFrame
#' @param area.weighted if not, ones vector will be return
#' 
#' @export
area.spatial <- function(grid, area.weighted = TRUE){
    if (area.weighted) {
        if (class(grid) == "SpatialPolygonsDataFrame") {
            area <- raster::area(grid)
        } else {
            grid2 <- grid[1, ] # SpatialGridDataFrame
            grid2@data <- data.frame(id = 1:nrow(grid2))
            r <- raster::raster(grid2)
            I <- r[[1]]@data@values %>% which.notna() # pixel becomes data.frame
            area <- raster::area(r)@data@values[I]
        }
    } else area <- rep(1, nrow(grid))
    area
}

# Statistic of median±sd or median
spatial_meansd <- function(x, area, stat, unit, FUN = weightedMedian){
    # mu <- median(x, na.rm = TRUE)
    fmt = "%.1f"
    if (!is.null(stat$digit)) fmt = sprintf("%%.%df", stat$digit)    
    if (is.null(FUN)) FUN = weightedMedian

    mu <- FUN(x, area, na.rm = TRUE) %>% sprintf(fmt, .)
    # weightedMedian, weightedMean
    sd <- weightedSd(x, area, na.rm = TRUE) %>% sprintf(fmt, .)

    unit2   = unit
    if(!(is.null(unit) || unit == "")) {
        unit2 <- unit
        # unit2 <- sprintf(" (%s)", unit)
    }

    lst.env = c(list(mu=mu, sd=sd, unit = unit2), stat)
    label <- if ( !is.null(stat$include.sd) && stat$include.sd ) {
        eval(substitute(expression(bolditalic(name) == mu * "±" * sd * " " * unit), lst.env))
    } else {
        eval(substitute(expression(bar(bolditalic(name)) == mu * " " * unit), lst.env))
    }

    label
}
