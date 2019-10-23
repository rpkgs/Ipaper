test_that("spplot_grid, levelplot2 works", {
    data("grid_avhrr")
    grid = grid_avhrr
    SpatialPixel = grid
    
    stat = list(show = TRUE, name="mean", loc = c(82.5, 26.5), digit = 1, include.sd = TRUE)
    pars = list(title = list(x=77, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=28, A=12, by = 0.4)) 
    
    # 1. spplot_grid
    expect_true({
        p <- spplot_grid(grid_avhrr, stat = stat, interpolate = FALSE)
        write_fig(p, "ex-spplot_grid.svg", 10, 7)
        TRUE
    })
    
    # 2. levelplot2
    expect_true({
        # data <- coordinates(grid) %>% as.data.table() %>% cbind(grid@data)
        d  = grid@data
        df = list(x = cbind(d, d), y = cbind(d, d)) %>% melt_list("type") %>% data.table()
        df = list(a = df, b = df) %>% melt_list("kind")
        
        df.mask = df[, .(mask = (X1982 > 270 | X1982 < 240)), .(type)]
        
        p <- levelplot2(X1982~s1+s2 | type + kind, df, SpatialPixel, 
                        # df.mask, 
                        aspect = 0.75, 
                        NO_begin = 3,
                        stat = stat, pars = pars, 
                        density = 0.5, interpolate = FALSE)
        write_fig(p, "ex-levelplot2.svg", 10, 7)
        TRUE
    })
    
    # 3. levelplot-shade
    expect_true({
        # data <- coordinates(grid) %>% as.data.table() %>% cbind(grid@data)
        d  = grid@data %>% data.table()
        df = d 
        df.mask = df[, .(mask = (X1982 > 270 | X1982 < 240))]
        p <- levelplot2(X1982~s1+s2, df, SpatialPixel, 
                        df.mask,
                        aspect = 0.75, 
                        NO_begin = 3,
                        stat = stat, pars = pars, 
                        density = 0.5, interpolate = FALSE)
        write_fig(p, "ex-levelplot2.svg", 10, 7)
        TRUE
    })
})
