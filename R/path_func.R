#' convert windows path to wsl
#'
#' In windows system, conversion will not be applied.
#' @inheritParams base::normalizePath
#'
#' @export
path.mnt <- function(path) {
    sep = substr(path, 2, 2)

    if (is_wsl()) {
        if (sep == ":") {
            pan  = substr(path, 1, 1) %>% tolower()
            path = sprintf("/mnt/%s/%s", pan, substr(path, 4, nchar(path)))
        }
    }
    path
}
