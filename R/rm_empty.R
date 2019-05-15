#' rm_empty
#' @param x A vector or list
#' 
#' @examples
#' # numeric
#' x <- c(1:5, NA)
#' rm_empty(x)
#' 
#' # list
#' l <- list(1:5, NULL, NA)
#' rm_empty(l)
#' @rdname tools
#' @export
rm_empty <- function(x){
    if (is.list(x)){
        x[sapply(x, length) > 0]
    }else {
        x[!is.na(x)]
    }
}
