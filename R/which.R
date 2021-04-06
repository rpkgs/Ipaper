#' which functions
#' @param x numeric vector
#' @export
which.na <- function(x) {
    which(is.na(x))
}

#' @export
#' @rdname which.na
which.notna <- function(x) {
    which(!is.na(x))
}

#' @export
#' @rdname which.na
which.isnull <- function(x) {
    which(sapply(x, is.null))
}

#' @rdname which.na
#' @export
which.notnull <- function(x) {
    which(!sapply(x, is.null))
}

last <- function (x, order_by = NULL, default = NA_real_) {
    nth(x, -1L, order_by = order_by, default = default)
}

first <- function(x, order_by = NULL, default = NA_real_) {
    nth(x, 1L, order_by = order_by, default = default)
}

nth <- function (x, n, order_by = NULL, default = NA_real_)  {
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
