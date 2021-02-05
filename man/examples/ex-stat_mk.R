library(ggplot2)

ggplot(mpg, aes(displ, hwy, colour = drv)) + 
    geom_point() + 
    stat_mk()
