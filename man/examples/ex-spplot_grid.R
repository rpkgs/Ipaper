data("grid_avhrr")

pars = list(title = list(x=77, y=39, cex=1.5), 
            hist = list(origin.x=77, origin.y=28, A=15, by = 0.4))
stat = list(show = TRUE, name="mean", loc = c(82.5, 26.5), digit = 1, include.sd = TRUE)

p <- spplot_grid(grid_avhrr, stat = stat, pars = pars, interpolate = FALSE)

write_fig(p, "ex-spplot_grid.svg", 9.8, 5)
