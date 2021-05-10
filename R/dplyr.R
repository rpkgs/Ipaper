#' @export
mutate <- function (.data, ...) 
{
    stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))
    cols <- as.list(substitute(list(...))[-1])
    cols <- cols[names(cols) != ""]
    for (col in names(cols)) {
        .data[[col]] <- eval(cols[[col]], .data, parent.frame())
    }
    .data
}

#' @export
transpose <- purrr::transpose
