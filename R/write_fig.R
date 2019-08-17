# save pdf just like `ggsave`
#' write_fig
#' 
#' show figure relies on SumatraPDF.exe, which needs add to path first.
#' 
#' @export
#' @importFrom grDevices svg tiff
#' @importFrom grDevices svg tiff
#' @export
write_fig <- function (p, file = "Rplot.pdf", width = 10, height = 5, res = 300, 
          show = TRUE) 
{
    if (missing(p)) 
        p <- last_plot()
    if ("grob" %in% class(p)) {
        FUN <- grid::grid.draw
    } else {
        FUN <- base::print
    }
    
    file_ext <- str_extract(basename(file), "(?<=\\.).{1,4}$")
    param <- list(file, width = width, height = height)
    if (file_ext == "pdf") {
        devicefun <- Cairo::CairoPDF
        param %<>% c(list(family = "Times"))
    }
    else if (file_ext == "svg") {
        devicefun <- svg
    }
    else if (file_ext == "emf") {
        devicefun <- grDevices::win.metafile
    }
    else {
        if (file_ext %in% c("tif", "tiff")) {
            devicefun <- tiff
        }
        else if (file_ext == "png") {
            devicefun <- Cairo::CairoPNG
        }
        param %<>% c(list(units = "in", res = res, compression = "lzw"))
    }
    do.call(devicefun, param)
    temp <- FUN(p)
    dev.off()
    
    if (show) {
        if (file.exists("/usr/sbin/rstudio-server") || file_ext %in% c("svg") ) {
            file.show(file)
        } else {
            app <- "SumatraPDF.exe"
            if (.Platform$OS.type == "unix") app <- "evince"
            cmd <- sprintf('%s \"%s\"', app, file)
            check_dir(dirname(file))

            tryCatch({
                status <- suppressWarnings(shell(cmd, intern = FALSE, wait = FALSE))
            }, error = function(e) {
                message(sprintf("[e] %s", e$message))
            })
        }
    }
}