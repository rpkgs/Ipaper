#' fprintf
#' Print sprintf result into console, just like C style fprintf function
#' @param fmt a character vector of format strings, each of up to 8192 bytes.
#' @param ... other parameters will be passed to `sprintf`
#' 
#' @examples
#' cat(fprintf("%s\n", "Hello phenofit!"))
#' @export
fprintf <- function(fmt, ...) cat(sprintf(fmt, ...))
