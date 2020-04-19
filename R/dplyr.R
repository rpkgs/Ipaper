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

#' @export
first <- function(x, order_by = NULL, default = NA_real_) {
    nth(x, 1L, order_by = order_by, default = default)
}

#' @export
last <- function(x, order_by = NULL, default = NA_real_) {
    nth(x, -1L, order_by = order_by, default = default)
}

#' @export
nth <- function (x, n, order_by = NULL, default = NA_real_) {
    stopifnot(length(n) == 1, is.numeric(n))
    n <- trunc(n)
    if (n == 0 || n > length(x) || n < -length(x)) {
        return(default)
    }
    if (n < 0) {
        n <- length(x) + n + 1
    }
    if (is.null(order_by)) {
        x[[n]]
    }
    else {
        x[[order(order_by)[[n]]]]
    }
}
