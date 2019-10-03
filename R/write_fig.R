# save pdf just like `ggsave`
#' write_fig
#' 
#' write plot to *.pdf, *.tif, *.png, *.jpg according to file suffix.
#' If show, pdf file will be shown in `SumatraPDF.exe`, which needs add to path first.
#' 
#' @param p could be one of `grid`, `ggplot` or `plot expression`
#' @param file file path of output figure
#' @inheritParams grDevices::svg
#' @inheritParams grDevices::png
#' 
#' @seealso [grDevices::cairo()], [grDevices::png()]
#' 
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
    } else if (file_ext == "svg") {
        devicefun <- svg
    } else if (file_ext == "emf") {
        devicefun <- switch(.Platform$OS.type, 
            "windows" = grDevices::win.metafile, 
            "unix" = devEMF::emf)
    } else {
        if (file_ext %in% c("tif", "tiff")) {
            devicefun <- tiff
        } else if (file_ext == "png") {
            devicefun <- Cairo::CairoPNG
        }
        param %<>% c(list(units = "in", res = res, compression = "lzw"))
    }
    
    do.call(devicefun, param)
    # print plot
    if (is.expression(p)) {
        eval(p, envir = parent.frame())
    } else {
        temp <- FUN(p)    
    }

    dev.off()
    
    app <- "SumatraPDF.exe"
    if (show) {
        if (file.exists("/usr/sbin/rstudio-server")) {
            if (file_ext == "pdf") {
                file.show(file)
            } else {
                # only suit for wsl mode
                cmd <- sprintf("%s \"%s\"", app, file)
                system(cmd, wait = FALSE)
            }
        } else {
            if (.Platform$OS.type == "unix") app <- "evince"
            if (file_ext %in% c("svg", "emf")) app <- ""
            cmd <- sprintf("%s \"%s\"", app, file)
        
            check_dir(dirname(file))
            tryCatch({
                status <- suppressWarnings(shell(cmd, intern = FALSE, wait = FALSE))
            }, error = function(e) {
                message(sprintf("[e] %s", e$message))
            })
        }
    }
}
