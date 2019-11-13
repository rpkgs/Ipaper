shell <- function(...){
    FUN <- switch(.Platform$OS.type, 
        "windows" = base::shell, 
        "unix" = base::system)
    FUN(...)
}

# windowsFonts(
#   Times = windowsFont("Times New Roman"), 
#   ST    = windowsFont("宋体"), 
#   YH    = windowsFont("Microsoft Yahei"), 
#   Arial = windowsFont("Arial"))

#' pal
#' show colors in figure device
#' 
#' @param col colors to be visualize.
#' @param border rect border for each color
#' 
#' @export
pal <- function(col, border = "light gray")
{
    n <- length(col)
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), 
         axes = FALSE, xlab = "", ylab = "")
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
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
    suppressWarnings(shell(cmd, ignore.stderr = TRUE, wait = FALSE))
}
