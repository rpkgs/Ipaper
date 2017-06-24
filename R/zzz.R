
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