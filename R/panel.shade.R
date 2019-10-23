# ' @examples

#' panel.shade
#'
#' @inheritParams graphics::polygon
#' @param ... 
#' - `panel.shade`     : other parameters to `polygon.fullhatch`;
#' - `panel.shade.list`: other parameters to [panel.shade()]
#' 
#' @author XiHui Gu
#' 
#' @example man/examples/ex-panel.shade.R
#' 
#' @seealso [graphics::polygon]
#' @export
panel.shade <- function(
    x, y = NULL, 
    density = NULL, angle = 45, border = NULL, 
    col = NA, lty = par("lty"), fill = NA, fillOddEven = FALSE, ...) 
{
    ..debug.hatch <- FALSE
    xy <- xy.coords(x, y, setLab = FALSE)
    if (is.numeric(density) && all(is.na(density) | density < 0))
        density <- NULL
    if (!is.null(angle) && !is.null(density)) {

        if (missing(col) || is.null(col) || is.na(col))
            col <- par("fg")
        if (is.null(border))
            border <- col
        if (is.logical(border)) {
            if (!is.na(border) && border)
                border <- col else border <- NA
        }
        start <- 1
        ends <- c(seq_along(xy$x)[is.na(xy$x) | is.na(xy$y)],
            length(xy$x) + 1)
        num.polygons <- length(ends)
        col <- rep_len(col, num.polygons)
        if (length(border))
            border <- rep_len(border, num.polygons)
        if (length(lty))
            lty <- rep_len(lty, num.polygons)
        if (length(density))
            density <- rep_len(density, num.polygons)
        angle <- rep_len(angle, num.polygons)
        i <- 1L
        for (end in ends) {
            if (end > start) {
                if (is.null(density) || is.na(density[i]) || density[i] < 0)
                    .External.graphics(C_polygon, 
                        xy$x[start:(end - 1)], 
                        xy$y[start:(end - 1)], 
                        col[i], NA, lty[i], ...) 
                else if (density[i] > 0) {
                    polygon.fullhatch(
                        xy$x[start:(end - 1)], 
                        xy$y[start:(end - 1)], 
                        col = col[i], lty = lty[i], density = density[i],
                        angle = angle[i], ..debug.hatch = ..debug.hatch, fillOddEven, ...)
                }
                i <- i + 1
            }
            start <- end + 1
        }
        # .External.graphics(C_polygon, xy$x, xy$y, NA, border, lty,
        # ...)
    } else {
        if (is.logical(border)) {
            if (!is.na(border) && border)
                border <- par("fg") else border <- NA
        }
        # .External.graphics(C_polygon, xy$x, xy$y, col, border, lty,
        # ...)
    }
    invisible()
}

#' @param lst List of coordinates matrix `[x, y]`
#' 
#' @rdname panel.shade
#' 
#' @importFrom iterators icount
#' @export
panel.shade.list <- function(lst, ...) {
    for (i in seq_along(lst)) {
        panel.shade(lst[[i]][, 1], lst[[i]][, 2], ...)        
    }
}

#' @rdname panel.shade
#' @export
panel.poly_grid <- function(s, union = TRUE, 
    density = 0.1, angle = 45, 
    col = "black", lwd = 1, lty = 1, border = NULL, ...) 
{
    if (union) {
        s <- union_polygon(s) #%T>% spplot()
    }
    
    lst_lonlat  <- coord.polygons(s)
    params <- listk(lst = lst_lonlat, density, angle, border, col, lty, lwd, ...)
    do.call(panel.shade.list, params)
}

#' @rdname panel.shade
#' @export
panel.polygonsplot2 <- function (
    x, y, z, subscripts, ..., 
    union = FALSE, density = 0.2, angle = 45, col = "grey65",lwd = 0.5, lty = 1, 
    poly_shade, sp.layout) 
{
    # sppanel(list(sp.layout), panel.number(), first = TRUE)
    # browser()
    # panel.gridplot, panel.polygonsplot
    panel.polygonsplot(x, y, z, subscripts, ..., 
                       col = col, lwd = lwd, lty = lty, sp.layout = sp.layout)
    # browser()
    panel.poly_grid(poly_shade, union, density, angle, 
                    col = col, lwd = lwd, lty = lty, sp.layout = NULL, ...)
    # sppanel(list(sp.layout), panel.number(), first = FALSE)
}

#' @rdname panel.shade
#' @export
panel.gridplot2 <- function (
    x, y, z, subscripts, ..., 
    union = FALSE, density = 0.2, angle = 45, col = "grey65", lwd = 0.5, lty = 1, 
    poly_shade, sp.layout) 
{
    # sppanel(list(sp.layout), panel.number(), first = TRUE)
    # browser()
    # panel.gridplot, panel.polygonsplot
    panel.gridplot(x, y, z, subscripts, ..., 
        col = col, lwd = lwd, lty = lty, sp.layout = sp.layout)
    # browser()
    panel.poly_grid(poly_shade, union, density, angle, 
        col = col, lwd = lwd, lty = lty, sp.layout = NULL, ...)
    # sppanel(list(sp.layout), panel.number(), first = FALSE)
}

#' @importFrom grid frameGrob placeGrob rectGrob segmentsGrob
#' @importFrom lattice panel.segments panel.points panel.arrows
polygon.onehatch <- function(x, y, x0, y0, xd, yd, ..debug.hatch = FALSE, fillOddEven, 
    ...) 
{
    if (..debug.hatch) {
        panel.points(x0, y0)
        panel.arrows(x0, y0, x0 + xd, y0 + yd)
    }
    halfplane <- as.integer(xd * (y - y0) - yd * (x - x0) <= 0)
    cross <- halfplane[-1L] - halfplane[-length(halfplane)]
    does.cross <- cross != 0
    if (!any(does.cross))
        return()
    x1 <- x[-length(x)][does.cross]
    y1 <- y[-length(y)][does.cross]
    x2 <- x[-1L][does.cross]
    y2 <- y[-1L][does.cross]
    t <- (((x1 - x0) * (y2 - y1) - (y1 - y0) * (x2 - x1))/(xd *
        (y2 - y1) - yd * (x2 - x1)))
    o <- order(t)
    tsort <- t[o]
    crossings <- cumsum(cross[does.cross][o])
    
    if (fillOddEven)
        crossings <- crossings%%2
    drawline <- crossings != 0
    lx <- x0 + xd * tsort
    ly <- y0 + yd * tsort
    lx1 <- lx[-length(lx)][drawline]
    ly1 <- ly[-length(ly)][drawline]
    lx2 <- lx[-1L][drawline]
    ly2 <- ly[-1L][drawline]
    panel.segments(lx1, ly1, lx2, ly2, ...)
}

polygon.fullhatch <- function(x, y, density, angle, ..debug.hatch = FALSE, fillOddEven, 
    ...) 
{
    x <- c(x, x[1L])
    y <- c(y, y[1L])
    angle <- angle%%180
    # if (par("xlog") || par("ylog")) {
    #     warning("cannot hatch with logarithmic scale active")
    #     return()
    # }
    usr <- par("usr")
    pin <- par("pin")
    upi <- c(usr[2L] - usr[1L], usr[4L] - usr[3L])/pin
    if (upi[1L] < 0)
        angle <- 180 - angle
    if (upi[2L] < 0)
        angle <- 180 - angle
    upi <- abs(upi)
    xd <- cos(angle/180 * pi) * upi[1L]
    yd <- sin(angle/180 * pi) * upi[2L]
    if (angle < 45 || angle > 135) {
        if (angle < 45) {
            first.x <- max(x)
            last.x <- min(x)
        } else {
            first.x <- min(x)
            last.x <- max(x)
        }
        y.shift <- upi[2L]/density/abs(cos(angle/180 * pi))
        x0 <- 0
        y0 <- floor((min(y) - first.x * yd/xd)/y.shift) * y.shift
        y.end <- max(y) - last.x * yd/xd
        while (y0 < y.end) {
            polygon.onehatch(x, y, x0, y0, xd, yd, ..debug.hatch = ..debug.hatch, fillOddEven, 
                ...)
            y0 <- y0 + y.shift
        }
    } else {
        if (angle < 90) {
            first.y <- max(y)
            last.y <- min(y)
        } else {
            first.y <- min(y)
            last.y <- max(y)
        }
        x.shift <- upi[1L]/density/abs(sin(angle/180 * pi))
        x0 <- floor((min(x) - first.y * xd/yd)/x.shift) * x.shift
        y0 <- 0
        x.end <- max(x) - last.y * xd/yd
        while (x0 < x.end) {
            polygon.onehatch(x, y, x0, y0, xd, yd, ..debug.hatch = ..debug.hatch, fillOddEven, 
                ...)
            x0 <- x0 + x.shift
        }
    }
}

#' @importFrom grid grid.segments
lsegments <- function (x0 = NULL, y0 = NULL, x1, y1, x2 = NULL, y2 = NULL, 
    col = add.line$col, alpha = add.line$alpha, lty = add.line$lty, 
    lwd = add.line$lwd, font, fontface, ..., identifier = NULL, 
    name.type = "panel") 
{
    if (missing(x0)) 
        x0 <- x2
    if (missing(y0)) 
        y0 <- y2
    add.line <- trellis.par.get("add.line")
    ml <- max(length(x0), length(x1), length(y0), length(y1))
    x0 <- rep(x0, length.out = ml)
    x1 <- rep(x1, length.out = ml)
    y0 <- rep(y0, length.out = ml)
    y1 <- rep(y1, length.out = ml)
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    grid.segments(x0 = x0, x1 = x1, y0 = y0, y1 = y1, 
        name = primName("segments", identifier, name.type, group), 
        gp = gpar(lty = lty, col = col, 
            lwd = lwd, alpha = alpha, ...), default.units = "native")
}

panel.segments <- function(...) lsegments(...)

hasGroupNumber <- function () {
    aname <- "group.number"
    fnames <- names(formals(sys.function(sys.parent())))
    if (is.na(match(aname, fnames))) {
        if (is.na(match("...", fnames))) 
            FALSE
        else {
            dotsCall <- eval(quote(substitute(list(...))), sys.parent())
            !is.na(match(aname, names(dotsCall)))
        }
    }
    else FALSE
}

primName <- function (name, identifier = NULL, name.type = "panel", 
    group = 0) 
{
    trellis.grobname(name = ifelse(is.null(identifier), name, 
        paste(identifier, name, sep = ".")), type = name.type, 
        group = group)
}
