#20161211 modified, avoid x length less then 5
#' Modified Mann Kendall
#'
#' If valid observations <= 5, NA will be returned.
#' 
#' Dongdong Kong
#' 
#' @param x numeric vector
#' @param ci critical value of autocorrelation
#' 
#' @return
#' * `Z0`   : The original (non corrected) Mann-Kendall test Z statistic. \cr
#' * `pval0`: The original (non corrected) Mann-Kendall test p-value \cr
#' * `Z`    : The new Z statistic after applying the correction \cr
#' * `pval` : Corrected p-value after accounting for serial autocorrelation \cr
#' `N/n*s` Value of the correction factor, representing the quotient of the number 
#' of samples N divided by the effective sample size `n*s` \cr
#' 
#' slp  : Sen slope, The slope of the (linear) trend according to Sen test
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
#' 
#' @examples
#' x <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' # r_cpp <- mkTrend_rcpp(x, IsPlot = TRUE)
#' r <- mkTrend(x)
#' @export
mkTrend <- function(x, ci = 0.95) {
    z = NULL
    z0 = NULL
    pval = NULL
    pval0 = NULL
    S = 0
    Tau = NULL
    essf = NULL
    
    # if (is.vector(x) == FALSE) stop("Input data must be a vector")
    I_bad <- !is.finite(x) # NA or Inf
    if (any(I_bad)) {
        x <- x[-which(I_bad)]
        # NA value also removed
        # warning("The input vector contains non-finite or NA values removed!")
    }

    n <- length(x)
    #20161211 modified, avoid x length less then 5, return rep(NA,5) c(z0, pval0, z, pval, slp)
    if (n < 5) return(rep(NA, 5))
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            S = S + sign(x[j] - x[i])
        }
    }
    ro <- acf(rank(lm(x ~ I(1:n))$resid), lag.max = (n - 1), plot = FALSE)$acf[-1]
    sig <- qnorm((1 + ci)/2)/sqrt(n)
    rof <- ifelse(abs(ro) > sig, ro, 0)#modified by dongdong Kong, 2017-04-03
    
    rof <- rep(NA, length(ro))
    for (i in 1:(length(ro))) {
        if (ro[i] > sig || ro[i] < -sig) {
            rof[i] <- ro[i]
        } else {
            rof[i] = 0
        }
    }
    cte <- 2/(n * (n - 1) * (n - 2))
    ess = 0
    for (i in 1:(n - 1)) {
        ess = ess + (n - i) * (n - i - 1) * (n - i - 2) * rof[i]
    }
    essf = 1 + ess * cte
    var.S = n * (n - 1) * (2 * n + 5) * (1/18)
    if (length(unique(x)) < n) {
        aux <- unique(x)
        for (i in 1:length(aux)) {
            tie <- length(which(x == aux[i]))
            if (tie > 1) {
                var.S = var.S - tie * (tie - 1) * (2 * tie + 5) * (1/18)
            }
        }
    }
    VS = var.S * essf
    if (S == 0) {
        z = 0
        z0 = 0
    }
    if (S > 0) {
        z = (S - 1)/sqrt(VS)
        z0 = (S - 1)/sqrt(var.S)
    } else {
        z = (S + 1)/sqrt(VS)
        z0 = (S + 1)/sqrt(var.S)
    }
    pval = 2 * pnorm(-abs(z))
    pval0 = 2 * pnorm(-abs(z0))
    Tau = S/(0.5 * n * (n - 1))
    V <- rep(NA, times = (n^2 - n)/2)
    k = 0
    for (i in 2:n) {
      for (j in 1:(i-1)){
        # for (j in 1:(n - 1)) {
            k = k + 1
            V[k] = (x[i] - x[j])/(i - j)
        }
    }
    slp <- median(na.omit(V))

    intercept <- mean(x - slp*seq_along(x), na.rm = T)
    c(z0 = z0, pval0 = pval0, z = z, pval = pval, slp = slp, intercept = intercept)
} 