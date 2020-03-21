#' pal
#' show colors in figure device
#' 
#' @param col colors to be visualize.
#' @param border rect border for each color
#' 
#' @export
pal <- function(col, border = "light gray")
{
    n <- length(col)
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), 
         axes = FALSE, xlab = "", ylab = "")
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

#' Construct lattice colorkey
#' 
#' @import RColorBrewer
#' @export
get_colorkey <- function(breaks, space = "bottom", lgd.title = NULL, is_factor = FALSE, 
    unit = NULL, unit.adj = 0.3, 
    cex = 1.3, fontfamily = "Times", fontface = 2)
{
    ncolor <- length(breaks) - 1
    cols <- colorRampPalette(c("firebrick1","orange3", "darkgoldenrod2", "grey90",
                               brewer.pal(9, "YlGnBu")[c(4, 6, 7)], "green4"))(ncolor) #,colors()[504]
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
        colorkey$labels$at <- breaks[-c(1, length(breaks))]
    }

    # colorkey$at <- at
    list(cols = cols, colorkey = colorkey)#return
}

# for levelplot2
get_colorkey2 <- function(breaks, space = "bottom", lgd.title = NULL, is_factor = FALSE, 
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
        labels = breaks
        at = seq_along(breaks) #+ 0.5
        labels_at = at

        if (breaks[1] == -Inf) {
            at[1]     = -Inf
            labels_at = labels_at[-1]
            labels    = labels[-1]
        } 
        if (breaks[length(breaks)] == Inf) {
            at[length(at)] = Inf
            n         = length(labels_at)
            labels_at = labels_at[-n]
            labels    = labels[-n]
        }
        colorkey$at <- at        
        colorkey$labels$at     <- labels_at
        colorkey$labels$labels <- labels
    } else {
        colorkey$labels$at <- breaks[-c(1, length(breaks))]
    }
    colorkey
}

get_break_colors <- function(colors, brks) {
    colfun <- colors %>% colorRampPalette()
    
    ncolor <- length(brks) - 1
    colfun(ncolor)
} 

get_break_colors2 <- function(colors, brks, is_factor = FALSE) {
    colfun <- colors %>% colorRampPalette()
    
    nbrk = length(brks)
    ncolor <- ifelse(is_factor, nbrk, nbrk - 1)
    colfun(ncolor)
} 
