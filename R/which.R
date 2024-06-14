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

#' @export
#' @rdname which.na
which.null <- which.isnull

#' @rdname which.na
#' @export
which.notnull <- function(x) {
    which(!sapply(x, is.null))
}

#' @export 
`%!in%` <- function(x, table) {
    !(x %in% table)
}

# ' @importFrom purrr is_empty
#' @rdname which.na
#' @export
which.notempty <- function(x) {
    which(!sapply(x, is_empty))
}

#' @rdname which.na
#' @export
which.empty <- function(x) {
    which(sapply(x, is_empty))
}

#' @rdname which.na
#' @export
which.dup <- function(x) {
  x[duplicated(x)]
}
