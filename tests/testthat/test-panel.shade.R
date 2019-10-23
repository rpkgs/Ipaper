test_that("panel.polygonsplot2 and panel.gridplot2 works", {
    library(sp)
    data("grid_avhrr")
    # library(grid)
        
    # set.seed(1)
    I <- c(1:2000, 4000:6000)
    r <- grid_avhrr
    poly   <- as(r[I, ], "SpatialPolygonsDataFrame")
    poly2  <- union_polygon(poly)
    
    expect_true({
        # print(spplot(poly, poly_shade = poly2, panel = panel.polygonsplot2, fill = "grey", col = "red"))
        print(spplot(r, poly_shade = poly, panel = panel.gridplot2, density = 1, fill = "grey", col = "red"))
        TRUE
    })
})
