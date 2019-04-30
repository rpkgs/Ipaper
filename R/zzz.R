#' @title Ipaper
#' @name Ipaper
#' @aliases Ipaper-package
#' @docType package
#' @keywords download paper DOI
#' 
#' @importFrom stringr str_extract
#' @importFrom jsonlite fromJSON read_json
#' @importFrom purrr is_empty map transpose
#' @importFrom methods as
#' @importFrom data.table data.table
#' @importFrom graphics rect
#' @import httr xml2 magrittr plyr
#' 
#' @useDynLib Ipaper, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
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

#' getwd_clip
#' 
#' get directory path in clipboard, same as getwd function
#' 
#' @references
#' https://stackoverflow.com/questions/10959521/how-to-write-to-clipboard-on-ubuntu-linux-in-r
#' @export
getwd_clip <- function(){
    if (.Platform$OS.type == 'windows') {
        path <- suppressWarnings(gsub("\\\\", "/", readLines("clipboard")))
        writeLines(path, "clipboard", sep = "")
        path#quickly return
    }
}

#' @title setwd_clip
#' @description set directory path in clipboard, same as setwd function
#' @export
setwd_clip <- function() setwd(getwd_clip())

#' fprintf
#' print sprintf result into console just like C style fprintf function
#' 
#' @param fmt a character vector of format strings, each of up to 8192 bytes.
#' @param ... values to be passed into fmt. Only logical, integer, real and 
#' character vectors are supported, but some coercion will be done: see the 
#' ‘Details’ section. Up to 100.
#' 
#' @export
fprintf <- function(fmt, ...) cat(sprintf(fmt, ...))

#' print the running ID in the console
#' 
#' @param i the running Id.
#' @param prefix prefix string
#' @param step how long of print step.
#' 
#' @rdname fprintf
#' @export
runningId <- function(i, prefix = "", step = 1) {
    if (mod(i, step) == 0) cat(sprintf("%s | running i=%5d ...\n", prefix, i))
}


#' @title makeVIDEO
#' @description make video through ffmpeg
#' 
#' @param file filename of output video
#' @param path The path of figures, i.e, pngs, to make video
#' @param pattern regular expression pattern to filter figures
#' @param mode must be one of "ultrafast", "slow", "veryslow"
#' 
#' @export
makeVIDEO <- function(file = "ffmpeg_%d.avi", 
                      path = ".", pattern = "*.png",
                      mode = c("ultrafast", "slow", "veryslow")){
  mode <- mode[1]
  system(sprintf("(for %%i in (%s\\%s) do @echo file '%%i') > list.txt", 
                gsub("/", "\\\\", path), pattern))
  system(sprintf("ffmpeg -safe 0 -f concat -r 2 -i list.txt -c:v libx264 -preset %s -crf 0 %s", 
                mode, gsub("/", "\\\\", file) ))
  file.remove("list.txt") #remove list.txt, return null
}
