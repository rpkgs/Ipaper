#' get factor frequency
#' 
#' @keywords internal
#' 
#' @examples
#' \dontrun{
#' get_perc.factor(factor(c(1, 2, 4), 1:4))
#' }
get_perc.factor <- function(z, w=NULL){
    if (is.null(w)) {
        z <- z[!is.na(z)] # rm NAN value
        d <- table(z) %>% as.data.frame()
        d$perc <- d$Freq/length(z)
    } else {
        df <- data.table(z, w)
        w_sum <- df[!is.na(z), sum(w)]
        d <- df[!is.na(z), .(Freq = .N, perc = sum(w)/w_sum), .(z)][order(z)]
    }

    zchr = d$z %<>% as.character()
    if (is.na(suppressWarnings(as.numeric(zchr[1])))) {
        d$z <- seq_along(zchr)
    } else {
        d$z <- as.numeric(zchr)
    }

    n_missing <- d$z[1] - 1    
    if (n_missing > 0) {
        d <- rbind(data.frame(z =1:n_missing, Freq = 0, perc = 0), d)     
    }
    d
}

#' panel.barchart2
#' 
#' new panel function for lattice, plot barchart in the bottomleft corner of subplot
#'
#' @param  {[type]}    perc      [description]
#' @param  {Number}    origin.x  [description]
#' @param  {Number}    origin.y  [description]
#' @param  {Number}    tck       [description]
#' @param  {Number}    dx        [description]
#' @param  {Number}    A         [description]
#' @param  {Number}    by        [description]
#' @param  {[type]}    col       Colors
#' @param  {[type]}    box.width [description]
#' @param  {Number}    ntick     [description]
#' @param  {Number}    text.cex  [description]
#' @param  {...[type]}           [description]
#' @return {[type]}              [description]
#' 
#' @importFrom lattice panel.arrows panel.segments panel.text panel.barchart
#' @export
panel.barchart2 <- function(z, subscripts, origin.x = 76, origin.y = 26.5, tck = 0.2, 
    A = 15, by = 0.6, 
    box.width = by*0.85, 
    axis.x.text = TRUE, axis.x.text.angle = 90,
    col, 
    fontfamily = "Times", border = "transparent", ntick = 2, 
    ylab.offset = 2.5, 
    tick = NULL,
    w = NULL, 
    text.cex = 1, 
    style = c("ZH", "EN"), ...)
{
    # TODO: add test
    
    dots <- list(...)
    col  <- dots$col.regions
    at   <- dots$at

    z <- z[subscripts]
    if (!is.null(at) & !is.factor(z)) {
        z <- cut(z, dots$at) %>% as.numeric()
    }
    
    # browser()
    perc <- get_perc.factor(z, w)$perc

    # tck <- 0.2#tick length
    ypos <- perc*A + origin.y 
    xpos <- seq(origin.x + by, by = by, length.out = length(perc))

    panel.barchart(x = xpos, y = ypos, horizontal = F, origin = origin.y, 
                 reference = F, col = col, box.width = box.width, border = border, ...)

    ymax <- ceiling(max(perc)*10)/10
    # ymax <- round(max(perc), 1)
    if (ymax <= 0.1) {
        ntick <- 1
    } else if (ymax <= 0.2){
        ntick <- 2
    }

    if (is.null(tick)) {
        tick <- pretty(c(0, ymax), ntick);
        if (ymax >= 0.5 && ymax <= 0.6) tick <- c(0, 0.3, 0.6)    
    }
    ymax <- max(tick) + 0.1

    tick_ypos <- tick*A + origin.y
    tick_xpos <- c(xpos - 0.6*by, max(xpos) + 0.5*by) 

    # avoid overlap
    delta_x = pmax(tck/3 - (by - box.width)/2 , 0)
    tick_xpos[1] <- tick_xpos[1]  - delta_x

    # axis and arrow
    panel.arrows(x0 = tick_xpos[1], y0 = origin.y, 
        c(tick_xpos[1], max(xpos)+1+tck*2),
        c(ymax*A + origin.y, origin.y), 
        col.line = 'black', type = 'closed', length = 0.05, col = 'black', fill = 'black', 
        identifier = "axis.arrow")
    # yaxis.tick
    panel.segments(
        x0 = tick_xpos[1], 
        x1 = tick_xpos[1] - tck,
        y0 = tick_ypos, 
        y1 = tick_ypos, identifier = "yaxis.tick"
    )
    # yaxis.text
    panel.text(x = tick_xpos[1]-tck-0.25*by, y = tick_ypos, 
        tick*100,
        fontfamily = fontfamily, 
        cex = text.cex, adj = 1, font = 2, identifier = "yaxis.text")

    # xaxis.tick.minor
    I <- seq(2, length(tick_xpos)-1, 2)
    panel.segments(
        x0 = tick_xpos[I], 
        x1 = tick_xpos[I],
        y0 = origin.y, 
        y1 = origin.y - tck/2, identifier = "xaxis.tick.minor"
    )
    # xaxis.tick.major
    if (length(tick_xpos) <= 6) {
        I <- seq(1, length(tick_xpos)-1)
    } else {
        # I <- seq(2, length(tick_xpos)-1, 2)
        I <- seq(2, length(tick_xpos)-1, 4)
    }
    panel.segments(
        x0 = tick_xpos[I], 
        x1 = tick_xpos[I],
        y0 = origin.y, 
        y1 = origin.y - tck, identifier = "xaxis.tick.major"
    )

    if (axis.x.text){
        adj <- c(0.5, 1)
        if (axis.x.text.angle == 90) adj <- c(1, 0.5)
        panel.text(x = tick_xpos[I], y = origin.y - tck*1.2, dots$brks[I],
            fontfamily = fontfamily, srt = axis.x.text.angle, 
            cex = text.cex, adj = adj, font = 2, identifier = "xaxis.text")    
    }
    
    fontfamily = get_family()
    ylab = ifelse(style[1] =="EN", "Fraction (%)", "频率 (%)")
    panel.text(tick_xpos[1] - ylab.offset, origin.y - tck, ylab, 
        srt = 90, font = 2, adj = c(0, 0.5), fontfamily = fontfamily, cex = 1.2, 
        identifier = "ylab.text")
}
