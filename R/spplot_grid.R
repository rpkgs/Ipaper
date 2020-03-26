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

# xlim <- c(73.5049, 104.9725)
# ylim <- c(25.99376, 40.12632)

#' Plot methods for spatial data with attributes
#' 
#' Lattice (trellis) plot methods for spatial data with attributes
#' 
#' @inheritParams levelplot2
#' @inheritParams sp::spplot
#' @param ... other parameters to spplot, for example:
#' - xlim
#' - ylim 
#' - ...
#' 
#' @example man/examples/ex-spplot_grid.R
#' 
#' @seealso [sp::spplot()], [lattice::levelplot()]
#' 
#' @importFrom matrixStats weightedMedian weightedSd
#' @importFrom sp spplot 
#' @importFrom grid frameGrob placeGrob rectGrob segmentsGrob polygonGrob 
#' @importFrom lattice panel.number panel.segments panel.points panel.arrows
#' 
#' @importFrom raster plot
#' @export
spplot_grid <- function(
    grid, zcols, 
    brks, colors, col.rev = FALSE, 
    toFactor = FALSE, 
    sub.hist = TRUE, 
    grob = NULL, bbox = c(0, 0.5, 0.5, 1),
    # xlim = NULL, #c(73.5049, 104.9725)
    # ylim = NULL, #c(25.99376, 40.12632)
    panel.title = NULL, 
    unit = "",  
    unit.adj = 0.3, 
    pars = list(title = list(x=77, y=39, cex=1.5), 
        hist = list(origin.x=77, origin.y=28, A=15, by = 0.4)), 
    stat = list(show = FALSE, name="RC", loc = c(81.5, 26.5), digit = 1, include.sd = FALSE),
    area.weighted = FALSE, 
    legend.space = "right", 
    layout = NULL,
    colorkey = TRUE, 
    interpolate = FALSE,
    lgd.title = NULL, 
    sp.layout = NULL, 
    par.settings = opt_trellis_default, 
    par.settings2 = list(axis.line = list(col = "transparent")),
    ...)
{
    # update par.settings (20191003)
    par.settings <- modifyList(par.settings, par.settings2)

    if (missing(zcols)) { zcols <- names(grid) }
    if (is.numeric(zcols)) { zcols <- names(grid)[zcols] }
    zcols %<>% intersect(names(grid@data))

    # statistic mean value 
    data.stat <- 
        if (stat$show && !is.null(stat$loc)) {
            area = area.spatial(grid, area.weighted)
            
            labels <- grid@data[, zcols, drop = FALSE] %>% map(spatial_meansd, area, stat, unit)
            list(loc = stat$loc, label = labels)
        } else NULL
    
    if (missing(colors)){ colors <- c("red", "grey80", "blue4") }

    if (missing(brks)) {
        vals <- grid@data[[zcols[1]]]
        range <- quantile(vals, c(0.05, 0.95), na.rm = TRUE)
        vals %<>% clamp(range)
        brks <- pretty(vals, n = 10) %>% c(-Inf, ., Inf)
        cols <- get_break_colors(colors, brks)
    } else {
        cols <- get_break_colors(colors, brks)        
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
    
    class  <- class(grid)
    params <- list(
        grid, zcols,
        col.regions = cols,
        panel.titles      = zcols,
        panel.titles_full = panel.title,
        panel = panel.spatial, 
        sub.hist = sub.hist,
        brks = brks,
        # xlim = xlim, ylim = ylim, 
        ...,
        strip = FALSE, as.table = TRUE,
        sp.layout = sp.layout,
        layout = layout,
        # drop.unused.levels = FALSE, 
        interpolate = interpolate,
        par.settings = par.settings,
        grob = grob, bbox = bbox,
        pars = pars,
        data.stat = data.stat,
        class = class
    )
    is_factor <- is.factor(grid@data[, zcols, drop = FALSE][[1]])
 
    if (!is_factor) params$at <- brks 
    
    if (colorkey) {
        params$colorkey <- get_colorkey(brks, NULL, legend.space, lgd.title, is_factor)
        params$colorkey$unit     = unit
        params$colorkey$unit.adj = unit.adj
    } else {
        params$colorkey <- FALSE
    }
    do.call(spplot, params)
}
