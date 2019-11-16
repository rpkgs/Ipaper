cmd_func <- function(command) {
    function (path = getwd()) {
        path <- normalizePath(path)
        cmd <- sprintf('%s "%s"', command, path)
        # if (.Platform$OS.type == "windows"){
        #     path <- gsub("/", "\\", path)
        # }
        shell(cmd)
    }
}

shell <- function(..., ignore.stderr = TRUE, wait = FALSE){
    FUN <- switch(.Platform$OS.type, 
        "windows" = base::shell, 
        "unix" = base::system)
    suppressWarnings(FUN(..., ignore.stderr = ignore.stderr, wait = wait))
    invisible()
}

#' code editor 
#' 
#' sublime text3 ad vscode
#' 
#' @keywords internal
#' @export
subl = cmd_func("subl")

#' @rdname subl
#' @export
code = cmd_func("code")

#' @rdname subl
#' @export
smerge = cmd_func("smerge")


#' merge_pdf
#'
#' rely on python pdfmerge package, `pip install pdfmerge`
#' 
#' @param outfile String
#' @param indir Directory to search pdfs
#' @param pattern string used to match pdf filename
#' @param del Booolean. If true, after merge, original pdf files will be delete.
#' 
#' @keywords internal
#' @examples
#' \dontrun{
#' merge_pdf("RPlot.pdf", indir = 'Figure', pattern = "*.pdf") 
#' }
#' @export
merge_pdf <- function(outfile = "RPlot.pdf", indir = 'Figure', pattern = "*.pdf", del = FALSE){
    files <- dir(indir, pattern, full.names = TRUE)
    cmd <- sprintf("pdfmerge -o %s %s", outfile, paste(files, collapse = " "))

    shell(cmd, wait = del)
    # system(sprintf('pdfmerge -o %s %s', outfile, pattern) )
    if (del) file.remove(files)
}

# display_tif <- function (data = NULL, file = NULL, width = NULL, height = NULL) 
# IRdisplay:::display_raw("image/tif", TRUE, data, file, IRdisplay:::img_metadata(width,  height))
#' jupyter display images
#' 
#' Only support jpeg, png, pdf and svg
#' 
#' @keywords internal
#' @importFrom IRdisplay display_jpeg display_png display_pdf display_svg
#' @export
display <- function(file, width = NULL, height = NULL, ...) {
    ext = tools::file_ext(file)
    FUN <- get(sprintf("display_%s", ext))
    FUN(file = file, width = width, height = height, ...)
}

#' @title dir.show
#' @name dir.show
#' 
#' @description open assign path in windows explorer, and default path is 
#' current directory. This function is only designed for windows system.
#' 
#' @param path the path you want to open
#' @export
dir.show <- function (path = getwd()) {
    path <- normalizePath(path)
    if (!dir.exists(path)) path %<>% dirname()

    cmd <- switch(.Platform$OS.type, 
        "windows" = paste("Explorer /e, ", gsub("/", "\\\\", path)), 
        "unix" = sprintf("nautilus '%s'", path))
    shell(cmd) 
}

#' check_dir
#' @param path character vectors 
#' 
#' @importFrom foreach %do%
#' @export
check_dir <- function(path){
    foreach(path_i = unique(path)) %do% {
        if (!dir.exists(path_i)){
            dir.create(path_i, recursive = TRUE)
        }    
    }
    path
}
