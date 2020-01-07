# z0       pval0           z        pval         slp   intercept 
# 0.293547633 0.769103596 0.545586542 0.585350178 0.002352941 4.636294118 

{
    y <- rep(c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69), 2)
}

# n = 32;
# fftwtools::fftw(y, n = 32)
profvis::profvis({
    for (i in 1:1e3) {
        # acf.fft(y)[-1]
        r_cpp <- mkTrend_rcpp(y, IsPlot = FALSE)
    }
})

microbenchmark::microbenchmark(
    mkTrend(y, IsPlot = FALSE),
    mkTrend_rcpp(y, IsPlot = FALSE), 
    times = 1e4
)
# 
# devtools::load_all()
# reprex::reprex({
# acf(lh, plot = FALSE)$acf[-1]
# acf.fft(lh)
# })
# ## Examples from Venables & Ripley
microbenchmark::microbenchmark(
    acf(y, plot = FALSE)$acf[-1],
    acf.fft(y)[-1], 
    times = 1e4
)


{
    set.seed(1)
    x = rnorm(100)
    
    microbenchmark::microbenchmark(
        r1 = rank(x, ties.method = "average"),
        r2 = frank(x, ties.method = "average"), 
        frankv(x),
        times = 1e2
    )
}

profvis::profvis(
    for (i in 1:1e3)
        rank(x, ties.method = "average")
)

# acf(lh, type = "covariance")
# pacf(lh)
