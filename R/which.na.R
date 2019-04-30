#' @export
which.na <- function(x){
    which(is.na(x))
}

#' @export
which.notna <- function(x){
    which(!is.na(x))
}
