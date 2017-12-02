
#' @title Ipaper
#' @name Ipaper
#' @aliases Ipaper-package
#' @docType package
#' @keywords download paper DOI
#' @importFrom stringr str_extract
#' @importFrom jsonlite fromJSON
#' @import httr xml2 magrittr
NULL

#' @title dir.show
#' @description open assign path in windows explorer, and default path is 
#' current directory. This function is only designed for windows system.
#' @export
dir.show <- function(path = getwd()){
  commandStr <- paste("Explorer /e, ", gsub("/", "\\\\", path))
  suppressWarnings(shell(commandStr))
}

#' @title fprintf
#' @description print sprintf result into console just like C style fprintf function
#' @export
fprintf <- function(fmt, ...) cat(sprintf(fmt, ...))

#' @title getwd_clip
#' @description get directory path in clipboard, same as getwd function
#' @export
getwd_clip <- function(){
  path <- suppressWarnings(gsub("\\\\", "/",readLines("clipboard")))
  writeLines(path, "clipboard", sep = "")
  path#quickly return
}

#' @title setwd_clip
#' @description set directory path in clipboard, same as setwd function
#' @export
setwd_clip <- function() setwd(getwd_clip())

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
  shell(sprintf("(for %%i in (%s\\%s) do @echo file '%%i') > list.txt", 
                gsub("/", "\\\\", path), pattern))
  shell(sprintf("ffmpeg -safe 0 -f concat -r 2 -i list.txt -c:v libx264 -preset %s -crf 0 %s", 
                mode, gsub("/", "\\\\", file)))
  file.remove("list.txt") #remove list.txt, return null
}

  