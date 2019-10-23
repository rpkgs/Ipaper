#' clamp
#' 
#' clamp values in the range of `lims`
#' 
#' @param x Numeric vector
#' @param lims limits
#' @param fill.na If true, values of lims are set to NA; else, values are just 
#' constrained in the range of `lims`.
#' 
#' @examples
#' clamp(1:10, lims = c(4, 7), fill.na = TRUE) 
#' @export
clamp <- function(x, lims = c(0, 1), fill.na = FALSE){
    if (fill.na) {
        x[x < lims[1]] <- NA_real_
        x[x > lims[2]] <- NA_real_
    } else {
        x[x < lims[1]] <- lims[1]
        x[x > lims[2]] <- lims[2]
    }
    x
}

#' @rdname clamp
#' @export
clamp_min <- function(x, value = 0){
    x[x < value] <- value
    x
}
