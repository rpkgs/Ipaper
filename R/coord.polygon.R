#' Get coordinates of spatialPolygons object
#' 
#' @param x Polygons class
#' @export
coord.polygon <- function(x){ 
    map(x@Polygons, ~.x@coords) %>% do.call(rbind, .)
}

#' coord.polygons
#' 
#' @param x spatialPolygons object
#' 
#' @return `lst_loc`
#' 
#' @rdname coord.polygon
#' @export
coord.polygons <- function(x) {
    polys <- x@polygons
    map(polys, coord.polygon)
}

#' @importFrom maptools unionSpatialPolygons
#' @export
union_polygon <- function(x){
    unionSpatialPolygons(x, rep(1, length(x)))
}
