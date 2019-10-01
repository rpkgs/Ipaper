# display_tif <- function (data = NULL, file = NULL, width = NULL, height = NULL) 
# IRdisplay:::display_raw("image/tif", TRUE, data, file, IRdisplay:::img_metadata(width,  height))

#' display images in jupyter
#' 
#' Only support jpeg, png, pdf and svg
#' 
#' @importFrom IRdisplay display_jpeg display_png display_pdf display_svg
#' @export
display <- function(file, width = NULL, height = NULL, ...) {
    ext = tools::file_ext(file)
    FUN <- get(sprintf("display_%s", ext))
    FUN(file = file, width = width, height = height, ...)
}
