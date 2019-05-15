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

#' @title dir.show
#' @description open assign path in windows explorer, and default path is 
#' current directory. This function is only designed for windows system.
#' 
#' @param path the path you want to open
#' @export
dir.show <- function(path = getwd()){
    if (.Platform$OS.type == 'windows'){
        commandStr <- paste("Explorer /e, ", gsub("/", "\\\\", path))
        suppressWarnings(shell(commandStr))  
    }
}

#' GET or SET working directory
#' 
#' @description 
#' * `getwd_clip`: get directory path in clipboard, same as getwd function
#' * `setwd_clip`: set directory path in clipboard, same as setwd function
#' 
#' @references
#' 1. https://stackoverflow.com/questions/10959521/how-to-write-to-clipboard-on-ubuntu-linux-in-r
#' @note Only works in windows
#' 
#' @examples
#' getwd_clip()
#' @export
getwd_clip <- function(){
    if (.Platform$OS.type == 'windows') {
        path <- suppressWarnings(gsub("\\\\", "/", readLines("clipboard")))
        writeLines(path, "clipboard", sep = "")
        path#quickly return
    }
}

#' @rdname getwd_clip
#' @export
setwd_clip <- function() setwd(getwd_clip())
