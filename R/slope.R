#' slope
#' 
#' @param y
#' 
#' @return c(intep, slope)
#' 
#' @export
slope <- function(y){
    if (!is.matrix(y)) y <- as.matrix(y)

    n <- nrow(y)
    I <- 1:n
    I_bad <- which(!is.finite(y)) # NA or Inf
    if (length(I_bad) > 0) {
        y <- y[-I_bad,]
        I <- I[-I_bad, ]
    }
    qr.solve(cbind(I*0+1, I) , y)[2, ] # only return slp
}
