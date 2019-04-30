#' function to separate data to steps of x, obtain 95 quantile value for smooth
#' @export
upper_envelope <- function(x, y, step = 0.2, alpha = 0.95){
    xrange <- range(x, na.rm = T)

    brks <- seq(xrange[1], xrange[2], by = step)
    n    <- length(brks)
    xmid <- (brks[-n] + brks[-1])/2

    brks[n] <- Inf

    res <- numeric(n-1)*NA_real_

    for (i in 1:(n-1)){
        val_min <- brks[i]
        val_max <- brks[i+1]

        I <- x >= val_min & x < val_max
        res[i] <- quantile(y[I], alpha, na.rm = T)
    }

    data.table(x = xmid, y = res)
}

#' ensemble_mean
#' 
#' @param FUN function of mean or median
#' 
#' @export
ensemble_mean <- function(x, y, step = 0.2, chunk=NULL, FUN = "mean") {
    FUN <- get(FUN, mode="function")

    xrange <- range(x, na.rm = T)

    # reorder x, y
    I <- order(x)
    x2 <- x[I]
    y2 <- y[I]

    N <- length(x)
    if (!is.null(chunk)){
        brks <- x2[seq(1, N, chunk)]
    } else {
        brks <- seq(xrange[1], xrange[2], by = step)
    }

    n    <- length(brks)
    xmid <- (brks[-n] + brks[-1])/2

    brks[n] <- Inf

    res <- numeric(n-1)*NA_real_

    for (i in 1:(n-1)){
        val_min <- brks[i]
        val_max <- brks[i+1]

        I <- x >= val_min & x2 < val_max
        res[i] <- FUN(y2[I], na.rm = T)
    }
    data.table(x = xmid, y = res)
}
