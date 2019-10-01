#' @import RColorBrewer
#' @export
get_colorkey <- function(breaks, space = "bottom", lgd.title = NULL, is_factor = FALSE, 
    unit = NULL, unit.adj = 0.3, 
    cex = 1.3, fontfamily = "Times", fontface = 2)
{
    ncolor <- length(breaks) - 1
    cols <- colorRampPalette(c("firebrick1","orange3", "darkgoldenrod2", "grey90",
                               brewer.pal(9, "YlGnBu")[c(4, 6, 7)], "green4"))(ncolor)#,colors()[504]
    # prepare for spplot
    colorkey <- list(
        title = lgd.title,
        labels = list(cex = cex, fontfamily = fontfamily, fontface = fontface),
        axis.line = list(col = 'black'),
        rect = list(col = "black", lwd = 0.4), 
        # tri.upper = TRUE,  tri.lower = TRUE, 
        height = 1, space = space, tck = 1, 
        unit = unit, unit.adj = unit.adj
    )

    if (is_factor) {
        at <- seq_along(breaks[-(1:2)]) + 0.5
        labels <- breaks[-c(1, length(breaks))]
        
        colorkey$labels$at     <- at
        colorkey$labels$labels <- labels
    } else {
        at <- breaks[-c(1, length(breaks))]
    }

    # colorkey$at <- at
    list(cols = cols, colorkey = colorkey)#return
}


check_brks <- function(brks){
    nbrk  <- length(brks)
    delta <- median(diff(brks))
    if (is.infinite(brks[1])) {
        brks[1] <- brks[2] - delta
    }

    if (is.infinite(brks[nbrk])) {
        brks[nbrk] <- brks[nbrk - 1] + delta
    }
    brks
}

#' @importFrom lattice panel.number panel.text 
#' @export
panel_hist <- function(x, y, z, subscripts, ...,  
    contour = FALSE, 
    grob = NULL, bbox, sub.hist = TRUE, sp.layout, 
    pars, class = NULL, 
    interpolate = TRUE, 
    data.stat = NULL)
{
    dot <- list(...)

    # print(str(listk(x, y, z, subscripts, ...)))#debug code
    sppanel(list(sp.layout), panel.number(), first = TRUE)

    if (is.null(class) || class == "SpatialPixelsDataFrame") {
        panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = interpolate)  
    } else {
        panel.polygonsplot(x, y, z, subscripts, ..., sp.layout = sp.layout)    
    }
    
    if (contour) {
        # dot$at <- c(5000)
        panel.levelplot(x, y, z, subscripts, 
            region = TRUE, contour = TRUE, labels = TRUE, 
            interpolate = FALSE)  
        # dot$region <- FALSE
        # params <- listk(x, y, z, subscripts, contour = TRUE, interpolate = FALSE, 
        #                 lwd = 0.1, lty =2,
        #                 labels = FALSE, label.style = "flat") %>% c(dot)
        # do.call(panel.contourplot, params)
        # ----------------------------------
        # params <- listk(x, y, z, subscripts, 
        #                 # interpolate = TRUE, label.style = "flat", 
        #                 lwd = 1, lty =1, col = "black", 
        #                 at = 1:8, 
        #                 # at = at,
        #                 contour = TRUE, region = FALSE, 
        #                 labels = TRUE) #%>% c(dot)
        # save(params, file = "debug.rda")
        # do.call(panel.levelplot, params)
    }

    # subplot
    if (!is.null(grob)) { panel.annotation(grob, bbox) }

    sppanel(list(sp.layout), panel.number(), first = FALSE)
    
    i <- ifelse(is.null(dot$order), panel.number(), dot$order)
    panel.title <- ifelse(is.null(dot$panel.title), 
                          paste0("(",letters[i], ") ", dot$panel.titles[i]), 
                          dot$panel.title[i])
    panel.text(pars$title$x, pars$title$y, panel.title, #english name: New_names[i])
               fontfamily = "Times", cex = pars$title$cex, font = 2, adj = 0)

    if (!is.null(data.stat)) {
        loc   <- data.stat$loc # 81.5, 26.5
        label <- data.stat$label[[i]]
        panel.text(loc[[1]], loc[[2]], label, fontfamily = "Times", cex = 1.2, adj = c(0.5, 0))    
    }

    if (sub.hist) {
        params <- listk(z, subscripts, ntick = 3, ...) %>% 
            c(., pars$hist)
        do.call(panel.barchart2, params)
    }
}

# xlim <- c(73.5049, 104.9725)
# ylim <- c(25.99376, 40.12632)

#' spplot_grid
#' 
#' 
#' @examples
#' \dontrun{
#' spplot_grid(grid, zcols, )
#' }
#' 
#' @importFrom sp spplot 
#' @importFrom grid frameGrob placeGrob rectGrob segmentsGrob
#' @importFrom lattice panel.number panel.segments panel.points panel.arrows
#' @export
spplot_grid <- function(
        grid, zcols, 
        brks, colors, col.rev = FALSE, 
        toFactor = FALSE, 
        sub.hist = TRUE, 
        grob = NULL, bbox = c(0, 0.5, 0.5, 1),
        xlim = c(73.5049, 104.9725), ylim = c(25.99376, 40.12632),
        pars = list(title = list(x=77, y=39, cex=1.5), 
            hist = list(origin.x=77, origin.y=28, A=15, by = 0.4)), 
        unit = "", 
        unit.adj = 0.3, 
        stat = list(show = FALSE, name="RC", loc = c(81.5, 26.5)),
        panel.title = NULL, 
        legend.space = "right", 
        layout = layout,
        colorkey = TRUE, 
        interpolate = TRUE,
        lgd.title = NULL, 
        sp.layout = NULL, ...)
{
    if (missing(zcols)) { zcols <- names(grid) }
    zcols %<>% intersect(names(grid@data))

    # statistic mean value 
    if (stat$show && !is.null(stat$loc)) {
        if (class(grid) == "SpatialPolygonsDataFrame") {
            area <- raster::area(grid)
        } else {
            area <- raster::area(raster::raster(grid))@data@values
        }

        labels <- grid@data[, zcols, drop = FALSE] %>% map(function(x){
            # mu <- median(x, na.rm = TRUE)
            mu <- weightedMedian(x, area, na.rm = TRUE) %>% sprintf("%.1f", .)
            # weightedMedian, weightedMean
            sd <- weightedSd(x, area, na.rm = TRUE) %>% sprintf("%.1f", .)

            unit2 = unit
            if(!(is.null(unit) || unit == "")) unit2 <- sprintf("(%s)", stat$unit)
            label <- eval(substitute(expression(bar(bolditalic(name)) == mu * unit), 
                c(list(mu=mu, sd=sd, unit = unit2), stat)))
            label
        })
        data.stat <- list(loc = stat$loc, label = labels)
    } else {
        data.stat <- NULL
    }

    if (missing(colors)){ colors <- c("red", "grey80", "blue4") }
    if (missing(brks)) {
        brks <- pretty(grid@data[[1]]) 
        cols <- colors
    } else {
        colfun <- colors %>% rep %>% colorRampPalette()
        ncolor <- length(brks) - 1
        cols <- colfun(ncolor) #%>% rev()    

        # cut into factor        
        df <- grid@data[, zcols, drop = FALSE]
        
        if (toFactor) {
            # drawkey can't support factor well
            df <- lapply(df, cut, brks) %>% as.data.frame()
        }
        levels <- cut(1, brks) %>% levels()
        grid@data <- df
    }
    if (col.rev) cols %<>% rev()   
    
    class <- class(grid)
    if (class == "SpatialPolygonsDataFrame") {
        w <- raster::area(grid)
    } else {
        w <- NULL
    }

    params <- list(
        grid, zcols,
        col.regions = cols,
        panel.titles = zcols, 
        panel = panel_hist, panel.title = panel.title,
        sub.hist = sub.hist, 
        brks = brks,
        xlim = xlim, ylim = ylim, ..., 
        strip = FALSE, as.table = TRUE,
        sp.layout = sp.layout, 
        layout = layout,
        drop.unused.levels = FALSE, 
        par.settings = opt_trellis, 
        grob = grob, bbox = bbox, 
        pars = pars,
        data.stat = data.stat, 
        class = class, 
        w = w
    )
    is_factor <- is.factor(grid@data[, zcols, drop = FALSE][[1]])
 
    if (!is_factor){ 
        params$at <- brks 
    }

    if (colorkey) {
        params$colorkey <- get_colorkey(brks, legend.space, lgd.title, is_factor)$colorkey
        params$colorkey$unit     = unit
        params$colorkey$unit.adj = unit.adj
    } else {
        params$colorkey <- FALSE
    }

    do.call(spplot, params)
}
