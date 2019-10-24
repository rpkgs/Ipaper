#' panel.spatial
#' 
#' @param par position of title, `list(title = list(x, y))`
#' 
#' @importFrom lattice panel.number panel.text 
#' @importFrom sp sppanel panel.gridplot panel.polygonsplot
#' @importFrom grid viewport popViewport grid.layout rasterGrob nullGrob
#' @export
panel.spatial <- function(x, y, z, subscripts, 
    ...,  
    contour = FALSE, 
    grob = NULL, bbox, sub.hist = TRUE, sp.layout, 
    pars, class = NULL, 
    interpolate = TRUE, 
    list.mask = NULL, 
    SpatialPixel = NULL,
    density = 1, angle = 45, 
    par.shade = NULL, 
    data.stat = NULL)
{
    NO_panel = panel.number()
    dot <- list(...)

    # print(str(listk(x, y, z, subscripts, ...)))#debug code
    sppanel(list(sp.layout), panel.number(), first = TRUE)

    if (is.null(class) || class == "SpatialPixelsDataFrame") {
        panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = interpolate)  
        # panel.levelplot.raster, panel.levelplot
    } else {
        panel.polygonsplot(x, y, z, subscripts, ..., sp.layout = sp.layout)    
    }
    
    if (contour)
        panel.levelplot(x, y, z, subscripts, 
            region = TRUE, contour = TRUE, labels = TRUE, interpolate = FALSE)  
    
    if (!is.null(list.mask) && !is.null(SpatialPixel)) {
        mask = list.mask[[NO_panel]]
        I_sign <- which(mask)
    
        par.shade %<>% as.list() %>% modifyList(options.spplot$shadePattern, .)
        poly_shade = raster2poly(SpatialPixel[I_sign, ])
        
        params.shade = list(poly_shade, union = FALSE, density, angle, sp.layout = NULL) %>% 
            c(., par.shade, ...)
        do.call(panel.poly_grid, params.shade)
    }

    ## 3. panel.annotation
    if (!is.null(grob)) { panel.annotation(grob, bbox) }
    sppanel(list(sp.layout), panel.number(), first = FALSE)
    
    ## 4. add panel title
    i <- ifelse(is.null(dot$NO_begin), 0, dot$NO_begin-1) + NO_panel
    
    panel.title <- ifelse(is.null(dot$panel.titles_full), 
                          paste0("(",letters[i], ") ", dot$panel.titles[NO_panel]), 
                          dot$panel.titles_full[NO_panel])
    panel.text(pars$title$x, pars$title$y, panel.title, #english name: New_names[i])
               fontfamily = "Times", cex = pars$title$cex, font = 2, adj = 0)

    ## 5. panel.text statistic values
    if (!is.null(data.stat)) {
        loc   <- data.stat$loc # 81.5, 26.5
        label <- data.stat$label[[NO_panel]]
        panel.text(loc[[1]], loc[[2]], label, fontfamily = "Times", cex = 1.2, adj = c(0, 0))    
    }

    ## 6. panel.hist
    if (sub.hist) {
        params <- listk(z, subscripts, ntick = 3, ...) %>% 
            c(., pars$hist)
        do.call(panel.barchart2, params)
    }
}

options.spplot <- list(
    shadePattern = list(col = "black", lwd = 1, lty = 1)
    )
