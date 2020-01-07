data("grid_avhrr")

{
    pars = list(title = list(x=77, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=28, A=15, by = 0.4))
    stat = list(show = TRUE, name="mean", loc = c(82.5, 26.5), digit = 1, include.sd = TRUE)
    
    p <- spplot_grid(grid_avhrr, 
                     stat = stat, 
                     pars = NULL, 
                     sub.hist = FALSE,
                     interpolate = FALSE)
    p2 <- levelplot2(X1982 ~ s1+s2, grid_avhrr@data, grid_avhrr)
    write_fig(p , "ex-spplot_grid.png", 9.8, 5)
    write_fig(p2, "ex-spplot_grid2.png", 9.8, 5)
}
