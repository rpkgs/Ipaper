---
title: "R语言通用图片保存函数`write_fig`"
output: 
    rmarkdown::html_vignette:
        toc: true
        fig_width: 7
        fig_height: 5
        dev: png
        keep_md: true
        number_sections: true
vignette: >
  %\VignetteIndexEntry{write_fig}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



你是否厌倦了，R语言每次保存图件时必须的三个步骤：先开dev，然后print图件，最后再dev.off?
你是否经常记混不同图片保存函数的参数。R语言中，图片保存函数众多，但不同的函数参数不尽相同。
```r
CairoPDF(file = ifelse(onefile, "Rplots.pdf","Rplot%03d.pdf"),
         width = 6, height = 6, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special", encoding, bg, fg, pointsize, pagecentre)
tiff(filename = "Rplot%03d.tif",
     width = 480, height = 480, units = "px", pointsize = 12,
     compression = c("none", "rle", "lzw", "jpeg", "zip", "lzw+p", "zip+p"),
     bg = "white", res = NA, family = "", restoreConsole = TRUE,
     type = c("windows", "cairo"), antialias)
CairoPNG, jpeg, svg, win.metafile, ...
```

今天要介绍的`Ipaper::write_fig`将这些图片保存函数统一起来。**`write_fig`支持保存成pdf, tif, png, jpg, svg和emf等任何你需要格式的图件。**生成不同类型的图片，不需要修改参数，只需要动动手指修改输出文件名的后缀即可。

# 安装方法


```r
devtools::install_git("https://gitee.com/adv-r/Ipaper")
```

# 使用说明
该函数的使用方法如下：


```r
library(Ipaper)
# ?write_fig # find the document
```


```r
write_fig(
  p,
  file = "Rplot.pdf",
  width = 10,
  height = 5,
  devices = NULL,
  res = 300,
  show = TRUE
)
```

**参数(arguments)**

* `p`: 可以是ggplot、lattice、grid或者基础绘图expression
* `file`: 输出图件的文件名
* `width`: 图片宽度（英寸in），默认10in
* `height`: 图片高度（英寸in），默认5in
* `devices`: 可以是以下的一种或多种c("pdf", "tif", "tiff", "png", "jpg", "svg", "emf")。如果没有提供，则从`file`中获取文件后缀。`file`无后缀，则默认为pdf.
* `res`: 图形分辨率，默认为论文格式要求300dpi。
* `show`: 布尔型，保存完后是否打开图件？

下面以不同的类型p的说明该函数的调用方法

## ggplot


```r
library(ggplot2)
p <- ggplot(mpg, aes(class, hwy)) + geom_boxplot2()
p
```

![](write_fig_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

`write_fig`图片默认保存的是pdf格式，pdf格式的图片是矢量图形，高清、易后期编辑是他的优点。如果`show=TRUE`，然后该函数会使用SumatraPDF打开输出的pdf。SumatraPDF是rstudio自带的pdf阅读器。SumatraPDF不像adobe或福昕，它不会锁死pdf文件。如果你想修改图片，重新导出pdf，你不需要事先在SumatraPDF中关掉重名的旧文件。


```r
write_fig(p, "fig_ggplot.pdf", show = TRUE)
```

如果你想保存其他格式的图片，只需将"fig_ggplot.pdf"的后缀pdf改成对应新图片类型的后缀即可，以tif为例：
```r
write_fig(p, "fig_ggplot.tif", show = TRUE)
```

如果你不想修改file名，你可以指定`devices`：
```r
write_fig(p, "fig_ggplot.pdf", show = TRUE, devices = "tif")
```

一次保存多种类型的图片：
```r
write_fig(p, "fig_ggplot.pdf", show = TRUE, devices = c("jpg", "png", "svg", "pdf", "tif", "emf"))
```

## lattice and grid

lattice和grid与ggplot类型的调用格式一致。

```r
library(lattice)
#> 
#> Attaching package: 'lattice'
#> The following object is masked from 'package:Ipaper':
#> 
#>     draw.colorkey
x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
p <- levelplot(z~x*y, grid, cuts = 50, scales=list(log="e"), xlab="",
          ylab="", main="Weird Function", sub="with log scales",
          colorkey = FALSE, region = TRUE)
p
```

![](write_fig_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
write_fig(p, "fig_lattice", show = TRUE)
```

grid类型

```r
g <- grid::circleGrob()
grid::grid.draw(g)
```

![](write_fig_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
write_fig(g, "fig_grid", show = TRUE)
```

## 基础绘图语法

对于基础绘图语法，你需要使用expression将其包裹起来。

```r
write_fig(expression({
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
}), "fig_expr.pdf")
```

![](write_fig_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

欢迎试用！感谢~
本文采用rmarkdown自动生成，欢迎关注该[repository](http://github.com/kongdd/Ipaper)。

本公众号定期推送R语言的最新教程，欢迎关注。
