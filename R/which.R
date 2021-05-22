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
