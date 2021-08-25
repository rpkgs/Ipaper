#' @export
transpose <- purrr::transpose

#' @importFrom dplyr first last mutate
#' @export
dplyr::first

#' @export
dplyr::last

#' @export
dplyr::mutate

#' @export
magrittr::`%>%`

#' @export
magrittr::`%<>%`

#' @export
magrittr::set_names

#' @export
purrr::map
# #' @importFrom progress progress_bar
# map <- function(.x, .f, ...) {
#     n = length(.x)
#     pb <- progress_bar$new(total = n)
#     .f <- as_mapper(.f, ...)
#     .f2 = function(..., .x = ..1, .y = ..2, . = ..1) {
#         pb$tick()
#         .f(..., .x, .y, .)
#     }
#     # browser()
#     .Call(purrr:::map_impl, environment(), ".x", ".f2", "list")
# }

#' @importFrom data.table data.table is.data.table as.data.table
#' @export
data.table::data.table

#' @export
data.table::is.data.table

#' @export
data.table::as.data.table

#' @importFrom lubridate make_date days_in_month
#' @export
lubridate::make_date

#' @export
lubridate::days_in_month

#' @importFrom foreach foreach %do% %dopar%
#' @export 
foreach::`%dopar%`

#' @export 
foreach::`%do%`

#' @import iterators
#' @export 
iterators::iter

#' @import matrixStats
#' @export
matrixStats::rowSums2

#' @export
matrixStats::rowMeans2

#' @export
matrixStats::rowQuantiles

#' @export
iterators::icount

# #' @export
# lubridate::date
