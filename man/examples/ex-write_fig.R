\dontrun{
library(ggplot2)
p <- ggplot(mpg, aes(class, hwy))
p1 <- p + geom_boxplot2()

## ggplot version
write_fig(p1, "Fig. 1. ggplot.pdf", show = TRUE)
write_fig(p1, "Fig. 1. ggplot", show = TRUE)
write_fig(p1, "Fig. 1. ggplot.pdf", show = TRUE, devices = c("jpg", "png", "svg", "pdf", "tif", "emf"))

## lattice
x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
p <- levelplot(z~x*y, grid, cuts = 50, scales=list(log="e"), xlab="",
          ylab="", main="Weird Function", sub="with log scales",
          colorkey = FALSE, region = TRUE)

write_fig(p, "fig_lattice", show = TRUE)

## grid expression
g <- grid::circleGrob()
write_fig(g, "fig_grid", show = TRUE)

## R expression
write_fig({
    rx <- range(x <- 10*1:nrow(volcano))
    ry <- range(y <- 10*1:ncol(volcano))
    ry <- ry + c(-1, 1) * (diff(rx) - diff(ry))/2
    tcol <- terrain.colors(12)
    # par(opar); 
    # opar <- par(pty = "s", bg = "lightcyan")
    plot(x = 0, y = 0, type = "n", xlim = rx, ylim = ry, xlab = "", ylab = "")
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = tcol[8], border = "red")
    contour(x, y, volcano, col = tcol[2], lty = "solid", add = TRUE,
            vfont = c("sans serif", "plain"))
    title("A Topographic Map of Maunga Whau", font = 4)
    abline(h = 200*0:4, v = 200*0:4, col = "lightgray", lty = 2, lwd = 0.1)
}, "fig_expr.pdf")
}