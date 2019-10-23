\dontrun{

library(sp2)
library(sp)
# library(grid)

range <- c(70, 140, 15, 55) # Tibetan Plateau
r  <- get_grid(range, cellsize = 1/2, midgrid = TRUE)

# set.seed(1)
I <- c(1:2000, 4000:6000)
poly   <- as(r[I, ], "SpatialPolygonsDataFrame")
poly2  <- union_polygon(poly)

expect_true({
    print(spplot(poly2 , poly_shade = poly2, panel = panel.polygonsplot2, fill = "grey", col = "red"))
    print(spplot(r[I, ], poly_shade = poly2, panel = panel.gridplot2    , fill = "grey", col = "red"))
    TRUE
})
}
