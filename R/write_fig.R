#' write figures into disk
#' 
#' write figure to pdf, tif, png, jpg, svg, or emf, according to file suffix.
#' 
#' @param p could be one of `grid`, `ggplot` or `plot expression`
#' @param file file path of output figure
#' @param devices can be c("pdf", "tif", "tiff", "png", "jpg", "svg", "emf"). If 
#' not specified, devices will be determined according to the postpix of `file`.
#' The default type is pdf.
#' @inheritParams grDevices::svg
#' @inheritParams grDevices::png
#' @param show Boolean. Whether show file after finished writing?
#' @param use.cairo_pdf This parameter is for pdf type. whether to use `grDevices::cairo_pdf`?
#' `cairo_pdf` supports self defined font, but can not create multiple page pdf.
#' 
#' @seealso [grDevices::cairo()], [grDevices::png()], [Cairo::Cairo()]
#' @example man/examples/ex-write_fig.R
#' 
#' @importFrom grDevices svg tiff
#' @export
write_fig <- function (p, file = "Rplot.pdf", width = 10, height = 5, 
    devices = NULL, res = 300, show = TRUE, use.cairo_pdf = FALSE) 
{
    ## begin of function -------------------------------------------------------
    showfig <- function(file) {
        file_ext = file_ext(file)
        app = ""
        if (.Platform$OS.type == "windows") 
            app = '"C:/Program Files/RStudio/bin/sumatra/SumatraPDF.exe"'
        if (.Platform$OS.type == "unix") app <- "evince"

        # if linux system
        is_server_pdf = file.exists("/usr/sbin/rstudio-server") && file_ext == "pdf"
        if (file_ext %in% c("svg", "emf", "jpg") || is_server_pdf) {
            file.show(file)
        } else {
            cmd = sprintf('"%s "%s" "', app, file)
            check_dir(dirname(file))
            tryCatch({
                status <- suppressWarnings(shell(cmd, intern = FALSE, wait = FALSE))
            }, error = function(e) {
                message(sprintf("[e] %s", e$message))
            })
        }
    }

    # open device for writing
    dev_open <- function(file, width, height, res, use.cairo_pdf = FALSE) {
        file_ext = file_ext(file)
        param <- list(file, width = width, height = height)

        if (file_ext == "pdf") {
            devicefun <- ifelse (use.cairo_pdf, cairo_pdf, Cairo::CairoPDF)
            param %<>% c(list(family = "Times"))
        } else if (file_ext == "svg") {
            devicefun <- svg
        } else if (file_ext == "emf") {
            devicefun <- switch(.Platform$OS.type, 
                "windows" = grDevices::win.metafile, 
                "unix" = devEMF::emf)
        } else {
            param %<>% c(list(units = "in", res = res))
            
            if (file_ext %in% c("tif", "tiff")) {
                devicefun <- tiff
                param$compression = "lzw"
            } else if (file_ext == "png") {
                devicefun <- Cairo::CairoPNG
            } else if (file_ext == "jpg") {
                devicefun <- jpeg
            } else {
                stop(sprintf("Unsupported type: %s", file_ext))
            }
        }
        # listk(devicefun, param)
        do.call(devicefun, param)
    }
    ## end of function -------------------------------------------------------

    if (missing(p)) p <- last_plot()
    expr  = substitute(p)
    class = class(expr)
    is_expr <- class %in% c("{")

    FUN <- base::print
    if (!is_expr) {
        if ("grob" %in% class(p)) {
            FUN <- grid::grid.draw
        } else {
            FUN <- base::print
        }    
    }
    
    outdir    = dirname(file)
    filename  = file_name(file)
    file_exts = if (is.null(devices)) file_ext(file) else devices
    if (length(file_exts) == 1 && is.na(file_exts)) file_exts = "pdf"

    env = parent.frame()
    for(i in seq_along(file_exts)) {
        file_ext = file_exts[i]
        outfile  = sprintf("%s/%s.%s", outdir, filename, file_ext)
        
        dev_open(outfile, width, height, res, use.cairo_pdf)
        # 1. print plot
        if (is_expr) {
            eval(expr, envir = env)
        } else {
            temp <- FUN(p)    
        }
        dev.off() # close device
        if (show) showfig(outfile)
    }
}
