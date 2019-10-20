#' slope
#' 
#' * `slope`     : linear regression slope
#' * `slope_p`   : linear regression slope and p-value
#' * `slope_mk`  : mann kendall Sen's slope and p-value
#' * `slope_boot`: bootstrap slope
#' 
#' @examples
#' y <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' r    <- slope(y)
#' r_p  <- slope_p(y)
#' r_mk <- slope_mk(y)
#' r_boot <- slope_boot(y)
#' @export
slope <- function(y, x){
    # TODO: add tests for slopew
    if (!is.matrix(y)) y <- as.matrix(y)
    n <- nrow(y)

    if (missing(x)) x <- 1:n
    I_bad <- which(!is.finite(y)) # NA or Inf
    if (length(I_bad) > 0) {
        y <- y[-I_bad,]
        x <- x[-I_bad, ]
    }
    slope <- qr.solve(cbind(x*0+1, x) , y)[2, ] # only return slp
    slope
    # set_names(slope, "slope")
}

#' @rdname slope
#' @export
slope_p <- function(y, x){
    if (!is.matrix(y)) y <- as.matrix(y)
    n <- nrow(y)

    if (missing(x)) x <- as.matrix(1:n)
    I_bad <- which(!is.finite(y)) # NA or Inf

    if (length(I_bad) > 0) {
        y <- y[-I_bad,]
        x <- x[-I_bad, ]
    }

    # foreach
    l <- lm(y~x)
    # pvalue: the smaller, the better
    info <- summary(l)
    info$coefficients[2, c(1, 4)] %>% set_names(c("slope", "pvalue"))
}

#' @rdname slope
#' @export
slope_mk <- function(x){
    mkTrend(x)[c('slp', 'pval')] %>% set_names(c("slope", "pvalue"))
}

#' @keywords internal
#' @rdname slope
#' @importFrom boot boot
#' @importFrom matrixStats colQuantiles colSds
#' @export
slope_boot <- function(y, slope_FUN = slope, times = 100, alpha = 0.1, seed) {
    if (!missing(seed)) set.seed(seed)
        
    x0  <- seq_along(y)
    FUN <- function(y0, indices) {
        y <- y0[indices]
        x <- x0[indices]
        slope_FUN(y, x)
    }
    b <- boot(y, FUN, R = times)
    
    probs  <- c(alpha/2, 0.5, 1 - alpha/2)
    b$coef <- colQuantiles(as.matrix(b$t), probs = probs, drop = FALSE) %>% 
        set_rownames(names(b$t0)) %>% 
        set_colnames(c("lower", "mean", "upper")) %>% 
        cbind(sd = colSds(b$t))
    b$coef
}
