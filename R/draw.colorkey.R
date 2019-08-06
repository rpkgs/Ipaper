#' @export
process.colorkey <- function(
    col = regions$col,
    alpha = regions$alpha,
    at,
    tick.number = 7,
    tck = 1,
    width = 2,
    height = 1,
    space = "right",
    raster = FALSE,
    interpolate = FALSE,
    tri.upper = NA,
    tri.lower = NA,
    title = NULL, 
    cex.title = 1,
    axis.line = list(),
    axis.text = list(),
    rect = list(col = "black", lwd = 0.3), # rect of legend
    ...)
{
    regions <- trellis.par.get("regions")
    list(col = col,
         alpha = alpha,
         at = at,
         tick.number = tick.number,
         tck = tck,
         width = width,
         height = height,
         space = space,
         raster = raster,
         interpolate = interpolate,
         tri.upper = tri.upper,
         tri.lower = tri.lower,
         title = title,
         cex.title = cex.title,
         axis.line = axis.line,
         axis.text = axis.text,
         rect = rect,
         ...)
}

#' convertTri
#'
#' Setting 'open.lower' and 'open.upper' to non-zero makes colorkey end with
#' triangular extensions, indicating open-ended intervals. Set to non-zero by
#' default only if first/last intervals are unbounded (-Inf / +Inf).
#' (NOTE: default should perhaps be 0 for back-compatibility, but currently
#' these are simply not shown in the legend, so probably new behaviour is no
#' worse).
#' When non-zero, controls fraction of key$height to be used for triangles at ends.
#' @export
convertTri <- function(x, inf = FALSE, height = 0.05)
{
    if (length(x) == 1) {
        if (is.numeric(x) && (x >= 0 && x <= 0.25)) {
            return(x)
        } else if (is.na(x)) {
            # return(0.05)
            return(height*inf)
        } else if (isTRUE(x)) {
            return(height)
        } else {
            return(0)
        }
    }
    warning("Invalid value of 'tri.upper/tri.lower' ignored.")
    0
}

#' draw.colorkey
#'
#' @inheritParams lattice::draw.colorkey
#'
#' @example man/examples/ex-draw.colorkey.R
#' @export
draw.colorkey <- function(key, draw = FALSE, vp = NULL)
{
    if (!is.list(key)) stop("key must be a list")
    key <- do.call(process.colorkey, key)

    axis.line <- updateList(trellis.par.get("axis.line"), key$axis.line)
    axis.text <- updateList(trellis.par.get("axis.text"), key$axis.text)

    # layout_name <- ifelse(key$space %in% c("top", "bottom"), "layout.heights", "layout.widths") 
    # colorkey.title.padding   <- lattice.options()[[layout_name]]$colorkey.title.padding
    # colorkey.title.padding$x <- colorkey.title.padding$x * 
    #     trellis.par.get(layout_name)$colorkey.title.padding
    ## made FALSE later if labels explicitly specified
    check.overlap <- TRUE

    ## Note: there are two 'at'-s here, one is key$at, which specifies
    ## the breakpoints of the rectangles, and the other is key$lab$at
    ## (optional) which is the positions of the ticks. We will use the
    ## 'at' variable for the latter, 'atrange' for the range of the
    ## former, and key$at explicitly when needed

    ## Getting the locations/dimensions/centers of the rectangles
    key$at <- sort(key$at) ## should check if ordered
    numcol <- length(key$at)-1
    ##     numcol.r <- length(key$col)
    ##     key$col <-
    ##         if (is.function(key$col)) key$col(numcol)
    ##         else if (numcol.r <= numcol) rep(key$col, length.out = numcol)
    ##         else key$col[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]

    key$col <- level.colors(x = seq_len(numcol) - 0.5,
                     at = seq_len(numcol + 1) - 1,
                     col.regions = key$col,
                     colors = TRUE)

    ## FIXME: need to handle DateTime classes properly
    atrange <- range(key$at, finite = TRUE)
    scat <- as.numeric(key$at) ## problems otherwise with DateTime objects (?)

    if (key$raster && !isTRUE(all.equal(diff(range(diff(scat))), 0)))
        warning("'at' values are not equispaced; output may be wrong")

    ## recnum <- length(scat)-1
    reccentre <- (scat[-1] + scat[-length(scat)]) / 2
    recdim <- diff(scat)

    cex  <- axis.text$cex
    col  <- axis.text$col
    font <- axis.text$font
    fontfamily <- axis.text$fontfamily
    fontface   <- axis.text$fontface
    lineheight <- axis.text$lineheight
    rot <- 0

    ## The following code assumes names key$lab and key$lab$lab (which
    ## may have been used in user code), whereas documentation says
    ## key$labels and key$labels$labels.  To make both work without
    ## 'partial matching' warnings, we rename key$labels to key$lab
    ## etc.

    if (!is.null(key[["labels"]])) {
        key[["lab"]] <- key[["labels"]]
        key[["labels"]] <- NULL
        if (is.list(key[["lab"]]) && !is.null(key[["lab"]][["labels"]])) {
            key[["lab"]][["lab"]] <- key[["lab"]][["labels"]]
            key[["lab"]][["labels"]] <- NULL
        }
    }

    if (is.null(key$lab)) {
        at <- lpretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- format(at, trim = TRUE)
    } else if (is.characterOrExpression(key$lab) && length(key$lab)==length(key$at)) {
        check.overlap <- FALSE
        at <- key$at
        labels <- key$lab
    } else if (is.list(key$lab)) {
        at <- if (!is.null(key$lab$at)) key$lab$at else lpretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- if (!is.null(key$lab$lab)) {
            check.overlap <- FALSE
            key$lab$lab
        } else format(at, trim = TRUE)
        if (!is.null(key$lab$cex)) cex <- key$lab$cex
        if (!is.null(key$lab$col)) col <- key$lab$col
        if (!is.null(key$lab$font)) font <- key$lab$font
        if (!is.null(key$lab$fontface)) fontface <- key$lab$fontface
        if (!is.null(key$lab$fontfamily)) fontfamily <- key$lab$fontfamily
        if (!is.null(key$lab$lineheight)) lineheight <- key$lab$lineheight
        if (!is.null(key$lab$rot)) rot <- key$lab$rot
    }
    else stop("malformed colorkey")

    labscat <- at
    do.labels <- (length(labscat) > 0)

    ## Tri
    height.Tri <- key$height/numcol
    open.lower <- convertTri(key$tri.lower, scat[1] == -Inf, height = height.Tri)
    open.upper <- convertTri(key$tri.upper, scat[length(scat)] == Inf, height.Tri)
    key.rect <- 1 - open.lower - open.upper

    # legend
    just = switch(key$space, 
        right  = if (rot == -90) c("center", "bottom") else c("left", "center"), 
        left   = if (rot == 90) c("center", "bottom") else c("right", "center"), 
        top    = if (rot == 0) c("center","bottom") else c("left", "center"),
        bottom = if (rot == 0) c("center", "top") else c("right", "center"))

    if (key$space %in% c('right', 'left')) {
        vp_label <- viewport(yscale = atrange)
        x_lab = rep(0, length(labscat))
        y_lab = labscat
    } else {
        vp_label <- viewport(xscale = atrange)
        y_lab = rep(0, length(labscat))
        x_lab = labscat
    }

    labelsGrob <-
        if (do.labels)
            textGrob(label = labels,
                     x = x_lab, y = y_lab, vp = vp_label,
                     default.units = "native",
                     check.overlap = check.overlap,
                     just = just, rot = rot,
                     name = trellis.grobname("labels", type="colorkey"),
                     gp = gpar(col = col, cex = cex,
                              fontfamily = fontfamily,
                              fontface = chooseFace(fontface, font),
                              lineheight = lineheight))
        else nullGrob()

    # layout
    grobwidth <- ifelse(key$space %in% c("top", "bottom"), "grobheight", "grobwidth")
    width_lab <- do.labels/length(labels)
    # For bottom and top, `lgd_width` is height
    widths.x    <- c(0.6*key$width, do.labels*(0.3 + key$tck*0.3), width_lab)
    widths_unit <- c("lines", "lines", grobwidth)   
    widths_data <- list(NULL, NULL, labelsGrob)
    
    lgd_width   <- unit(widths.x, widths_unit, data = widths_data) # for 'right' and 'bottom'
    if (key$space %in% c('left', 'top')) lgd_width <- rev(lgd_width)
    
    heights.x <- c(0.5*(1 - key$height),
                 key$height*c(open.upper, key.rect, open.lower),
                 0.5*(1 - key$height))
    lgd_height <- unit(heights.x, rep("null", 5))

    if (key$space %in% c("right", "left")) {
        key.layout <- grid.layout(nrow = 5, ncol = 3, respect = TRUE,
                        heights = lgd_height,
                        widths = lgd_width)
    } else if (key$space %in% c("top", "bottom")) {
        key.layout <- grid.layout(nrow = 3, ncol = 5, respect = TRUE,
                        heights = lgd_width,
                        widths  = lgd_height)
    }

    gp.border <- with(axis.line,
        gpar(col = col, lty = lty, lwd = lwd, alpha = alpha, fill = "transparent"))

    key.gf <- key_gf(key, key.layout, vp, vp_label, axis.line, reccentre, recdim, FALSE)
    key.gf <- key_triangle(key.gf, key, open.lower, open.upper)
    key.gf <- key_border(key.gf, key, open.lower, open.upper, gp.border)
    key.gf <- key_label(key.gf, key, labscat, labelsGrob, vp_label, axis.line)

    if (draw) grid.draw(key.gf)
    key.gf
}

environment(draw.colorkey) <- environment(lattice::xyplot)
assignInNamespace("draw.colorkey", draw.colorkey, ns="lattice")

# updateList <- function(x, val)
# {
#     if (is.null(x)) x <- list()
#     modifyList(x, val)
# }

# is.characterOrExpression <- function(x){
#     is.character(x) || is.expression(x) || is.call(x) || is.symbol(x)
# }

# lpretty <- function(x, ...){
#     eps <- 1e-10
#     at <- pretty(x[is.finite(x)], ...)
#     ifelse(abs(at-round(at, 3))<eps, round(at, 3), at)
# }

# chooseFace <- function(fontface = NULL, font = 1) {
#     if (is.null(fontface)) font else fontface
# }
