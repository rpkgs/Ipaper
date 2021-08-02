
add_gridLine <- function(dates, col = "grey60", lty = 3, ...) {
    years <- year(dates)
    date_beg <- ymd( min(years) *1e4 + 0101 )
    date_end <- ymd( max(years) *1e4 + 0101 )
    
    t_grids  <- seq.Date(date_beg, date_end, by = "year")
    abline(v = t_grids, col = col, lty = lty, ...)
}

make_dt <- function(..., ncol = 3) {
    x <- list(...)
    n <- length(x)
    nrow <- floor(n / ncol)
    lapply(1:nrow, function(i) {
        ind <- seq((i - 1) * ncol + 1, i * ncol)
        x[ind] %>% as.data.table()
    }) %>% do.call(rbind, .)
}
