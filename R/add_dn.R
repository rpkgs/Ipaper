#' Add n-day flag
#'
#' To aggregated data into n-day (e.g. 8-day, 16-day) like MODIS product, a
#' n-day flag is need.
#'
#' @param d data.frame or data.table
#' @param days Integer number or vector, can't have duplicated value.
#'
#' @examples
#' date <- seq.Date(as.Date("2010-01-01"), as.Date("2010-12-31"), by = "day")
#' d <- data.frame(date)
#' dnew <- add_dn(d, days = c(8, 16))
#' @importFrom lubridate ymd year yday
#' @export
add_dn <- function(d, days = 8) {
  if (class(d$date) != "Date") {
    d$date %<>% ymd()
  }

  d %<>% dplyr::mutate(d, year = year(date), doy = yday(date))

  days <- floor(days)
  for (i in seq_along(days)) {
    day <- days[i]
    # d$d8  = ceiling(d$doy/8)
    eval(parse(text = sprintf("d$d%d <- ceiling(d$doy/%d)", day, day)))
  }
  return(d)
}
