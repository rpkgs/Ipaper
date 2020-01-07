# 20161211 modified, avoid x length less then 5
#' Modified Mann Kendall
#'
#' If valid observations <= 5, NA will be returned.
#' 
#' mkTrend is 4-fold faster with `.lm.fit`.
#' 
#' @param x numeric vector
#' @param ci critical value of autocorrelation
#'
#' @return
#' * `Z0`   : The original (non corrected) Mann-Kendall test Z statistic.
#' * `pval0`: The original (non corrected) Mann-Kendall test p-value
#' * `Z`    : The new Z statistic after applying the correction
#' * `pval` : Corrected p-value after accounting for serial autocorrelation
#' `N/n*s` Value of the correction factor, representing the quotient of the number
#' of samples N divided by the effective sample size `n*s`
#' * `slp`  : Sen slope, The slope of the (linear) trend according to Sen test
#' 
#' @note
#' slp is significant, if pval < alpha.
#' 
#' @references
#' Hipel, K.W. and McLeod, A.I. (1994),
#' \emph{Time Series Modelling of Water Resources and Environmental Systems}.
#' New York: Elsevier Science.
#'
#' Libiseller, C. and Grimvall, A., (2002), Performance of partial
#' Mann-Kendall tests for trend detection in the presence of covariates.
#' \emph{Environmetrics} 13, 71--84, \url{http://dx.doi.org/10.1002/env.507}.
#'
#' @seealso `fume::mktrend` and `trend::mk.test`
#' @author Dongdong Kong 
#' 
#' @examples
#' x <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
#' r_cpp <- mkTrend(x, IsPlot = TRUE)
#' 
#' @importFrom data.table frank
#' @export
mkTrend_rcpp <- function(x, ci = 0.95, IsPlot = FALSE) {
    z0    = z = NA_real_
    pval0 = pval = NA_real_
    slp <- NA_real_
    intercept <- NA_real_

    if (IsPlot) {
        plot(x, type = "b")
        grid()
        rlm <- lm(x~seq_along(x))
        abline(rlm$coefficients, col = "blue")
        legend("topright", c('MK', 'lm'), col = c("red", "blue"), lty = 1)
    }
    names(x) <- NULL

    # if (is.vector(x) == FALSE) stop("Input data must be a vector")
    I_bad <- !is.finite(x) # NA or Inf
    if (any(I_bad)) {
        x <- x[-which(I_bad)]
        # NA value also removed
        # warning("The input vector contains non-finite or NA values removed!")
    }
    n <- length(x)
    #20161211 modified, avoid x length less then 5, return rep(NA,5) c(z0, pval0, z, pval, slp)
    if (n < 5) {
        return(c(z0 = z0, pval0 = pval0, z = z, pval = pval, slp = slp, intercept = intercept))
    }

    S = Sf(x) # call cpp

    resid = .lm.fit(cbind(matrix(1:n, ncol = 1), 1), x)$residuals 
    resid %<>% rank()
    # resid = lm(x ~ I(1:n))$resid
    # ro <- acf(resid, lag.max = (n - 1), plot = FALSE)$acf[-1]
    ro  <- acf.fft(resid, lag.max = (n - 1))[-1]
    sig <- qnorm((1 + ci)/2)/sqrt(n)
    rof <- ifelse(abs(ro) > sig, ro, 0) # modified by dongdong Kong, 2017-04-03

    temp = varS(x, rof, S)
    z  = temp["z"][[1]]
    z0 = temp["z0"][[1]]
    
    pval = 2 * pnorm(-abs(z))
    pval0 = 2 * pnorm(-abs(z0))
    Tau = S/(0.5 * n * (n - 1))

    slp <- senslope(x)
    intercept <- mean(x - slp * seq_along(x), na.rm = T)
    if (IsPlot) abline(b = slp, a = intercept, col = "red")
    
    c(z0 = z0, pval0 = pval0, z = z, pval = pval, slp = slp, intercept = intercept)
} 

senslope_r <- function(x) {
    n = length(x)
    V <- rep(NA, times = (n^2 - n) / 2)
    k = 0
    for (i in 2:n) {
        for (j in 1:(i - 1)) {
            # for (j in 1:(n - 1)) {
            k = k + 1
            V[k] = (x[i] - x[j]) / (i - j)
        }
    }
    median(na.omit(V))
}

Sf_r <- function(x) {
    n = length(x)
    S = 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            S = S + sign(x[j] - x[i])
        }
    }
    S
}

#' faster autocorrelation based on ffw
#' 
#' This function is 4-times faster than [stats::acf()]
#' 
#' @keywords internal
#' 
#' @references 
#' 1. https://gist.github.com/FHedin/05d4d6d74e67922dfad88038b04f621c
#' 2. https://gist.github.com/ajkluber/f293eefba2f946f47bfa
#' 3. http://www.tibonihoo.net/literate_musing/autocorrelations.html#wikispecd
#' 4. https://lingpipe-blog.com/2012/06/08/autocorrelation-fft-kiss-eigen
#' 
#' @importFrom fftwtools fftw
#' @export
acf.fft <- function(x, lag.max = NULL)
{
    N <- length(x)
    if (is.null(lag.max)) 
        lag.max = N - 1
    else lag.max = pmin(lag.max, N - 1)
    
    # get a centred version of the signal
    x <- x - mean(x)
    #  need to pad with zeroes first ; pad to a power of 2 will give faster FFT
    m = ceiling(log2(N))
    len.opt <- 2^m - N
    # len.opt = 0

    x0 <- c(x, rep.int(0,len.opt))

    # fft using fast fftw library as backend
    FF  <- fftw(x0)
    FF2 <- (abs(FF))^2

    # browser()
    # take the inverse transform of the power spectral density
    FF_inv <- fftw(FF2, inverse=1)
    # browser()
    # browser()
    # We repeat the same process (except for centering) on a ‘mask’ signal,
    # in order to estimate the error made on the previous computation.
    #   mask <- c(rep.int(1,N), rep.int(0,len.opt))
    #   mask <- fftw(mask)
    #   mask <- fftw((abs(mask))^2,inverse=1)
    #   acf <- FF_inv / mask

    # The “error” made can now be easily corrected by an element-wise division
    acf <- FF_inv
    # keep real parts only (although there should be not imaginary part or really small ones) and remove padding data
    acf <- Re(acf[1:N])
    # now what we have is autocovariance, for getting autocorrelation
    # The normalization is made by the variance of the signal,
    # which corresponds to the very first value of the autocovariance.
    acf[1:(lag.max+1)]/acf[1]
}

rank <- function (x, na.last = TRUE, ties.method = c("average", 
    "first", "last", "random", "max", 
    "min")) 
{
    nas <- is.na(x)
    nm <- names(x)
    ties.method <- match.arg(ties.method)
    # print(ties.method)

    if (is.factor(x)) 
        x <- as.integer(x)
    x <- x[!nas]
    y <- switch(ties.method, 
        average = , min = , 
        max     = .Internal(rank(x, length(x), ties.method)), 
        first   = sort.list(sort.list(x)), 
        last    = sort.list(rev.default(sort.list(x, decreasing = TRUE))), 
        random  = sort.list(order(x, stats::runif(sum(!nas))))
    )
    if (!is.na(na.last) && any(nas)) {
        yy <- NA
        NAkeep <- (na.last == "keep")
        if (NAkeep || na.last) {
            yy[!nas] <- y
            if (!NAkeep) 
                yy[nas] <- (length(y) + 1L):length(yy)
        }
        else {
            len <- sum(nas)
            yy[!nas] <- y + len
            yy[nas] <- seq_len(len)
        }
        y <- yy
        names(y) <- nm
    }
    else names(y) <- nm[!nas]
    y
}
