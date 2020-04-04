#' Plot methods for spatial data with attributes
#' 
#' Lattice (trellis) plot methods for spatial data with attributes
#' 
#' @inheritParams lattice::levelplot
#' @inheritParams sp::spplot
#' 
#' @param sub.hist Boolean. If false, hist will be eliminated.
#' @param xlim,ylim The limits of x and y
#' @param pars parameters controlling hist, e.g. `list(title = list(x=77, y=39, cex=1.5), 
#' hist = list(origin.x=77, origin.y=28, A=15, by = 0.4))`.
#' 
#' @param formula a formula of the form z ~ s1 + s2 | g1 * g2 * ..., where z is a 
#' numeric response, and x, y are numeric values evaluated on a rectangular grid. 
#' g1, g2, ... are optional conditional variables, and must be either factors or 
#' shingles if present.
#' @param df data.table object, with columns e.g. lon, lat, and others
#' @param df.mask NULL or same length data.table as df, with the columns of `mask`
#' and same group variabes as `df`. 
#' `mask` is a boolean vector, which is used to distinguish significant pixels.
#' Note that factor levels should be same for grouped variables in `df` and `df.mask`.
#' 
#' If `mask` present in `df`, `df.mask` will be ignored.
#' @param colorkey Boolean or list returned by [get_colorkey()].
#' @param NO_begin beginning NO of the first panel
#' 
#' @example man/examples/ex-spplot_grid.R
#' 
#' @seealso [spplot_grid()], [sp::spplot()], [lattice::levelplot()]
#' @note parameter `panel.title` change to `panel.titles_full`
#' - `panel.titles_full` is for tags.
#' - `strip.factors` is for strip factors
#' 
#' @importFrom matrixStats weightedMedian weightedSd
#' @importFrom sp spplot coordinates 
#' @importFrom grid frameGrob placeGrob rectGrob segmentsGrob polygonGrob 
#' @importFrom lattice panel.number panel.segments panel.points panel.arrows
#' @importFrom data.table as.data.table
#' @export
levelplot2 <- function(
    formula, 
    df,
    SpatialPixel,
    df.mask = NULL,
    # grid, zcols, 
    brks, colors, col.rev = FALSE, 
    strip = FALSE,
    toFactor = FALSE, 
    grob = NULL, bbox = c(0, 0.5, 0.5, 1),
    # xlim = c(73.5049, 104.9725), ylim = c(25.99376, 40.12632),
    xlim = NULL, ylim = NULL, 
    strip.factors = NULL, 
    panel.titles_full = NULL, 
    unit = "",  
    unit.adj = 0.3, 
    pars = NULL, 
    stat = list(show = FALSE, name="RC", loc = c(81.5, 26.5), digit = 1, include.sd = FALSE, FUN = weightedMedian),
    stat_sign = list(loc1 = c(78, 41), loc2 = c(78, 41-2)),
    sub.hist = TRUE, 
    area.weighted = FALSE, 
    legend.space = "right", 
    layout = NULL,
    colorkey = TRUE, 
    legend.num2factor = FALSE, 
    interpolate = FALSE,
    lgd.title = NULL, 
    sp.layout = NULL, 
    NO_begin = 1, 
    cex.lgd = 1.3, 
    par.settings = opt_trellis_default, 
    par.settings2 = list(axis.line = list(col = "white")),
    ...)
{
    if (is.null(pars)) sub.hist = FALSE

    info.formula = parse.formula(formula)
    value.var = info.formula$value.var
    groups    = info.formula$groups
    
    # zcols only for one group
    zcols = if (length(groups) == 1) {
        levels <- levels(df[[groups]])
        labs_unique = unique(df[[groups]])
        if (is.null(levels)) {
            levels <- labs_unique 
        } else {
            levels <- intersect(levels, labs_unique)
        }
        levels
    }else NULL

    npixel = nrow(SpatialPixel)
    par.settings <- modifyList(par.settings, par.settings2)

    if (is.null(df.mask) && "mask" %in% colnames(df)) {
        df.mask <- df
    }

    list.mask = NULL
    labels_sign = NULL
    if (!is.null(df.mask)) {
        ## make sure factor is the same
        for (i in seq_along(groups)) {
            varname = groups[i]
            levels = levels(df[[varname]])
            if (is.null(levels)) levels = unique(df[[varname]])
            df.mask[[varname]] %<>% factor(levels = levels)
        }
        list.mask = dlply(df.mask, rev(groups), function(d) d$mask)

        ## the percentage of significant or not
        labels_sign = dlply(df, rev(groups), function(d) {
            val <- sign(d[[value.var]])
            mask <- d$mask
            tbl <- table(mask, val)
         
            N <- sum(as.numeric(tbl))
            perc <- tbl / N * 100
            str_neg <- sprintf("N: %.1f%% (%.1f%%)", sum(perc[, 1]), perc[2, 1])
            str_pos <- sprintf("P: %.1f%% (%.1f%%)", sum(perc[, 2]), perc[2, 2])
            data.table(str_neg, str_pos)
        })
    }
    stat_sign$data <- labels_sign

    # statistic mean value 
    data.stat <- 
        if (stat$show && !is.null(stat$loc)) {
            area = area.spatial(SpatialPixel, area.weighted)
            # need to debug for two variables group
            labels = dlply(df, rev(groups), function(d) 
                spatial_meansd(d[[value.var]], area, stat, unit, FUN = stat$FUN))
            list(loc = stat$loc, label = labels)
        } else NULL

    is_factor <- is.factor(df[[value.var]])
    
    if (missing(colors)){ colors <- c("red", "grey80", "blue4") }
    if (missing(brks)) {
        if (!is_factor) {
            vals  <- df[[value.var]]
            range <- quantile(vals, c(0.05, 0.95), na.rm = TRUE)
            vals %<>% clamp(range)                
            brks <- pretty(vals, n = 10) %>% c(-Inf, ., Inf)
        } else {
            brks = levels(df[[value.var]])
        }
        cols <- get_break_colors2(colors, brks, is_factor)
    } else {
        cols <- get_break_colors2(colors, brks, is_factor)
        if (toFactor) df[[value.var]] %<>% cut(brks) # cut into factor
        levels <- cut(1, brks) %>% levels()
    }
    if (col.rev) cols %<>% rev()   
    
    class  <- class(SpatialPixel)
    data   <- coordinates(SpatialPixel) %>% as.data.table() %>% cbind(df)

    if (strip == TRUE) {
        n = length(zcols)
        if(is.null(strip.factors)) strip.factors <- zcols
        # names <- if (is.null(strip.factors)) zcols else strip.factors
        strip_levels = label_tag(strip.factors)
        strip = strip.custom(factor.levels=strip_levels)
        zcols = NULL
    }

    params <- list(
        formula, data,
        list.mask = list.mask, 
        SpatialPixel = SpatialPixel, 
        ...,
        stat_sign   = stat_sign, 
        col.regions = cols,
        panel.titles = zcols,
        panel.titles_full = panel.titles_full,
        panel = panel.spatial, 
        NO_begin = NO_begin,
        sub.hist = sub.hist,
        brks = brks,
        # xlim = xlim, ylim = ylim, 
        strip = strip, 
        as.table = TRUE,
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
    if (!is.null(xlim)) params$xlim <- xlim
    if (!is.null(ylim)) params$ylim <- ylim
    
    nbrk = length(brks)
    params$at <- if (!is_factor) brks else seq(0.5, nbrk+1)
    if (is.list(colorkey) || colorkey) {
        is_factor2 = legend.num2factor || is_factor
        colorkey.param <- get_colorkey(brks, NULL, legend.space, lgd.title, is_factor2, cex = cex.lgd)        
        colorkey.param$unit     = unit
        colorkey.param$unit.adj = unit.adj
        
        if (is.list(colorkey)) colorkey.param %<>% updateList(colorkey)
        params$colorkey <- colorkey.param
    } else {
        params$colorkey <- FALSE
    }

    do.call(levelplot, params)
}
