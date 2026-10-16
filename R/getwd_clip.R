#' @title GET or SET working directory
#' @name getwd_clip
#'
#' @description
#' * `getwd_clip`: get directory path in clipboard, same as getwd function
#' * `setwd_clip`: set directory path in clipboard, same as setwd function
#'
#' @references
#' 1. https://stackoverflow.com/questions/10959521/how-to-write-to-clipboard-on-ubuntu-linux-in-r
#'
#' @note Uses the system clipboard on Windows, macOS and Linux.
#'
#' @examples
#' \dontrun{
#' getwd_clip()
#' setwd_clip()
#' dir.show()
#' }
#' @import clipr
#' @export
getwd_clip <- function() {
  path <- suppressWarnings(read_clip())
  if (is_win()) path <- gsub("\\\\", "/", path)
  write_clip(path)
  path
}

#' @rdname getwd_clip
#' @export
setwd_clip <- function() setwd(getwd_clip())
