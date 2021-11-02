#' The corresponding date of the first day of month
#' 
#' @export
date_ym <- function(date) make_date(year(date), month(date))

#' @export
#' @rdname date_ym
date_y <- function(date) make_date(year(date))
