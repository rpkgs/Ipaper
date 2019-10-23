#' panel.annotation
#' 
#' @param grob grid object
#' @param bbox `[xmin, xmax, ymin, ymax]`
#' @param ... ignored
#' 
#' @importFrom grid grid.draw pushViewport grid.grabExpr
#' @export
panel.annotation <- function(grob, bbox = c(0, 0.5, 0.5, 1), ...) {
    width  <- diff(bbox[1:2])
    height <- diff(bbox[3:4])
    x <- bbox[1] + width/2
    y <- bbox[3] + height/2
    
    pushViewport(viewport(x, y, width, height, name="panel.annotation"))
    grid.draw(grob)
    
    popViewport()
}
