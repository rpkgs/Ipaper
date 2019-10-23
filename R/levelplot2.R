#' Plot methods for spatial data with attributes
#' 
#' Lattice (trellis) plot methods for spatial data with attributes
#' 
#' @inheritParams lattice::levelplot
#' @inheritParams sp::spplot
#' 
#' @param formula a formula of the form z ~ x * y | g1 * g2 * ..., where z is a 
#' numeric response, and x, y are numeric values evaluated on a rectangular grid. 
#' g1, g2, ... are optional conditional variables, and must be either factors or 
#' shingles if present.
#' @param df data.table object, with columns e.g. lon, lat, and others
#' @param df.mask NULL or same length data.table as df, with the columns of `mask`
#' and same group variabes as `df`. Mask is used to distinguish significant pixels.
#' 
#' @param NO_begin beginning NO of the first panel
#' 
#' @example man/examples/ex-spplot_grid.R
#' 
#' @seealso [spplot_grid()], [sp::spplot()], [lattice::levelplot()]
#' 
#' @importFrom matrixStats weightedMedian weightedSd
#' @importFrom sp spplot coordinates 
#' @importFrom grid frameGrob placeGrob rectGrob segmentsGrob polygonGrob 
#' @importFrom lattice panel.number panel.segments panel.points panel.arrows
#' @export
levelplot2 <- function(
    formula, 
    df,
    SpatialPixel,
    df.mask = NULL,
    # grid, zcols, 
    brks, colors, col.rev = FALSE, 
    toFactor = FALSE, 
    sub.hist = TRUE, 
    grob = NULL, bbox = c(0, 0.5, 0.5, 1),
    xlim = c(73.5049, 104.9725), ylim = c(25.99376, 40.12632),
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
    interpolate = TRUE,
    lgd.title = NULL, 
    sp.layout = NULL, 
    NO_begin = 1, 
    par.settings = opt_trellis_default, 
    par.settings2 = list(axis.line = list(col = "white")),
    ...)
{
    info.formula = parse.formula(formula)
    value.var = info.formula$value.var
    groups    = info.formula$groups
    
    npixel = nrow(SpatialPixel)
    par.settings <- modifyList(par.settings, par.settings2)

    list.mask = if (!is.null(df.mask)) {
        dlply(df.mask, groups, function(d) d$mask)
    } else NULL

    # statistic mean value 
    data.stat <- 
        if (stat$show && !is.null(stat$loc)) {
            area = area.spatial(SpatialPixel, area.weighted)
            # need to debug for two variables group
            labels = dlply(df, groups, function(d) spatial_meansd(d[[value.var]], area, stat, unit))
            list(loc = stat$loc, label = labels)
        } else NULL
    
    if (missing(colors)){ colors <- c("red", "grey80", "blue4") }
    if (missing(brks)) {
        vals  <- df[[value.var]]
        range <- quantile(vals, c(0.05, 0.95), na.rm = TRUE)
        vals %<>% clamp(range)
        brks <- pretty(vals, n = 10) %>% c(-Inf, ., Inf)
        cols <- get_break_colors(colors, brks)
    } else {
        cols <- get_break_colors(colors, brks)
        if (toFactor) df[[value.var]] %<>% cut(brks) # cut into factor
        levels <- cut(1, brks) %>% levels()
    }
    if (col.rev) cols %<>% rev()   
    
    class  <- class(SpatialPixel)
    data   <- coordinates(SpatialPixel) %>% as.data.table() %>% cbind(df)
    params <- list(
        formula, data,
        list.mask = list.mask, 
        SpatialPixel = SpatialPixel, 
        ...,
        col.regions = cols,
        # panel.titles = zcols,
        panel.titles_full = panel.title,
        panel = panel.spatial, 
        NO_begin = NO_begin,
        sub.hist = sub.hist,
        brks = brks,
        xlim = xlim, ylim = ylim, 
        strip = FALSE, as.table = TRUE,
        sp.layout = sp.layout,
        layout = layout,
        scales = list(draw = FALSE),
        xlab = NULL, ylab = NULL, 
        # drop.unused.levels = FALSE, 
        interpolate = interpolate,
        par.settings = par.settings,
        grob = grob, bbox = bbox,
        pars = pars,
        data.stat = data.stat,
        class = class
    )
    is_factor <- is.factor(df[[value.var]])
    if (!is_factor) params$at <- brks 
    
    if (colorkey) {
        params$colorkey <- get_colorkey(brks, legend.space, lgd.title, is_factor)$colorkey
        params$colorkey$unit     = unit
        params$colorkey$unit.adj = unit.adj
    } else {
        params$colorkey <- FALSE
    }
    # browser()
    do.call(levelplot, params)
}
