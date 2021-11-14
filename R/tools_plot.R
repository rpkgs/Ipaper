#' add_gridLine
#' 
#' @param dates Date vector
#' @inheritParams graphics::abline
#' @param ... others to [graphics::abline()]
#' 
#' @export
add_gridLine <- function(dates, col = "grey60", lty = 3, ...) {
    years <- year(dates)
    date_beg <- ymd( min(years) *1e4 + 0101 )
    date_end <- ymd( max(years) *1e4 + 0101 )
    
    t_grids  <- seq.Date(date_beg, date_end, by = "year")
    abline(v = t_grids, col = col, lty = lty, ...)
}
