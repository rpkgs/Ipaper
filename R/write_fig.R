# save pdf just like `ggsave`
#' write_fig
#' 
#' show figure relies on SumatraPDF.exe, which needs add to path first.
#' 
#' @export
write_fig <- function(p, file = "Rplot.pdf", width = 10, height = 5, res = 300, show = T){
    if (missing(p)) p <- last_plot()

    if ("grob" %in% class(p)) {
        FUN <- grid::grid.draw
    } else{
        FUN <- base::print
    }

    file_ext <- str_extract(basename(file), "(?<=\\.).{1,4}$")

    param <- list(file, width = width, height = height)
    if (file_ext == "pdf"){
        devicefun <- cairo_pdf # Cairo::CairoPDF #
    } else if (file_ext == "svg"){
        devicefun <- svg
    } else if (file_ext == "emf") {
        devicefun <- grDevices::win.metafile
    } else {
        if (file_ext %in% c("tif", "tiff")){
            devicefun <- tiff
        } else if (file_ext == "png") {
            devicefun <- Cairo::CairoPNG
        }
        param %<>% c(list(units = "in", res = res, compression = "lzw")) #, dpi = 300
    }

    # print(FUN)
    # Cairo::CairoPDF, if only one figure cairo_pdf is the best
    do.call(devicefun, param)
    FUN(p)
    dev.off()
    if (show) {
        cmd <- sprintf('SumatraPDF.exe "%s"', file)
        status <- suppressWarnings(shell(cmd, intern=FALSE, wait=FALSE))
    }
}
