# deprecated
# kongdd, 
#' @export
panel.polygonsplot.shade <- function (
    x, y, z, subscripts, 
    density = NULL, angle = 45, border = NULL, 
    show.shade = TRUE,
    at = pretty(z), shrink, labels = NULL, 
    label.style = c("mixed", "flat", "align"), 
    contour = FALSE, 
    region = TRUE, col = add.line$col, lty = add.line$lty, 
    lwd = add.line$lwd, cex = add.text$cex, font = add.text$font, 
    fontfamily = add.text$fontfamily, fontface = add.text$fontface, 
    col.text = add.text$col, ..., col.regions = regions$col, 
    alpha.regions = regions$alpha, grid.polygons, sp.layout) 
{
    regions  <- trellis.par.get("regions")
    add.line <- trellis.par.get("add.line")
    add.text <- trellis.par.get("add.text")
    numcol   <- length(at) - 1
    numcol.r <- length(col.regions)
    col.regions <- if (numcol.r <= numcol) 
        rep(col.regions, length = numcol)
    else col.regions[floor(1 + (1:numcol - 1) * (numcol.r - 1)/(numcol - 1))]
    zcol <- rep(NA, length(z))
    for (i in seq(along = col.regions)) zcol[!is.na(x) & !is.na(y) & 
        !is.na(z) & z >= at[i] & z < at[i + 1]] <- i
    label.style <- match.arg(label.style)
    x <- as.numeric(x[subscripts])
    y <- as.numeric(y[subscripts])
    z <- as.numeric(z[subscripts])
    zcol <- as.numeric(zcol[subscripts])
    
    sppanel(list(sp.layout), panel.number(), first = TRUE)

    if (any(subscripts)) {
        if (is(grid.polygons, "SpatialLines")) {
            sp.lines3 = function(x, col, ...) panel.lines(coordinates(x), 
                col = col, ...)
            sp.lines2 = function(x, col, ...) lapply(x@Lines, 
                sp.lines3, col, ...)
            for (i in 1:length(grid.polygons@lines)) sp.lines2(grid.polygons@lines[[i]], 
                col = col.regions[zcol[i]], lwd = lwd, lty = lty, 
                ...)
        } else {
            pls = slot(grid.polygons, "polygons")
            pO = slot(grid.polygons, "plotOrder")
            col = rep(col, length.out = length(grid.polygons))

            for (i in pO) {
                # if (get_Polypath()) {
                #     print("d1")

                #     obj = as(as(grid.polygons[i, ], "SpatialLines"), "SpatialPointsDataFrame")
                #     cc = coordinates(obj)
                #     id = as.numeric(obj$Line.NR)
                #     fill = col.regions[zcol[i]]
                #     alpha = alpha.regions
                #     grid.path(cc[, 1], cc[, 2], id, default.units = "native", 
                #         gp = gpar(col = col[i], fill = fill, alpha = alpha, 
                #             lwd = lwd, lty = lty, ...))
                # } else {
                    Srs <- slot(pls[[i]], "Polygons")
                    pOi <- slot(pls[[i]], "plotOrder")

                    for (j in pOi) {
                        coords = slot(Srs[[j]], "coords")
                        # browser()
                        
                        if (slot(Srs[[j]], "hole")) {
                            bg    = trellis.par.get()$background
                            fill  = ifelse (bg$col == "transparent", fill = "white", bg$col)                         
                            alpha = bg$alpha
                        } else {
                            fill  = col.regions[zcol[i]]
                            alpha = alpha.regions
                        }
                        gp = gpar(fill = fill, alpha = alpha, col = col, lwd = lwd, lty = lty)

                        if (show.shade){
                            grid.polygon(coords[, 1], coords[, 2], default.units = "native", gp = gp)    
                        }
                        panel.polylines(coords[, 1], coords[, 2], 
                            density, angle, border, 
                            fill = fill, alpha = alpha, col = col, lwd = lwd, lty = lty, ...)
                    }
                # }
            }
        }
    }

    sppanel(list(sp.layout), panel.number(), first = FALSE)
}

# grid.polygon <- function (x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL, 
#     id.lengths = NULL, default.units = "npc", name = NULL, 
#     gp = gpar(), draw = TRUE, vp = NULL) 
# {
#     pg <- polygonGrob(x = x, y = y, id = id, id.lengths = id.lengths, 
#         default.units = default.units, name = name, gp = gp, 
#         vp = vp)
#     if (draw) 
#         grid.draw(pg)
#     invisible(pg)
# }

# polygonGrob <- function (x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL, 
#     id.lengths = NULL, default.units = "npc", name = NULL, 
#     gp = gpar(), vp = NULL) 
# {
#     if (!is.unit(x)) 
#         x <- unit(x, default.units)
#     if (!is.unit(y)) 
#         y <- unit(y, default.units)
#     grob(x = x, y = y, id = id, id.lengths = id.lengths, name = name, 
#         gp = gp, vp = vp, cl = "polygon")
# }
