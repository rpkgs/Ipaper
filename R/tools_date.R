#' The corresponding date of the first day of month
#' 
#' @export
date_ym <- function(date) make_date(year(date), month(date))

#' @export
#' @rdname date_ym
date_y <- function(date) make_date(year(date))

#' @export
#' @rdname date_ym
date_yj <- function(year, doy) {
  make_date(year) + doy - 1
}

#' @export
#' @rdname date_ym
date_ydn <- function(year, dn, delta = 8) {
  doy = (dn - 1) * delta + 1
  make_date(year) + doy - 1
}
