#' @export
transpose <- purrr::transpose

#' @importFrom plyr mapvalues revalue
#' @export
plyr::mapvalues

#' @export
plyr::revalue

#' @importFrom dplyr first last mutate top_n top_frac as_tibble tribble
#' @export
dplyr::first

#' @export
dplyr::last

#' @export
dplyr::mutate

#' @export
dplyr::top_n

#' @export
dplyr::top_frac

#' @export
dplyr::as_tibble

#' @export 
dplyr::tribble

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

#' @importFrom data.table data.table is.data.table as.data.table fread fwrite
#' @export
data.table::data.table

#' @export
data.table::is.data.table

#' @export
data.table::fread

#' @export
data.table::fwrite

#' @export
print.data.table <- function(d, ...) {
  cat("[data.table]: \n")
  print(as_tibble(d))
}

#' @export
data.table::as.data.table

#' @importFrom lubridate make_date make_datetime days_in_month month year day
#' @export
lubridate::make_date

#' @export
lubridate::make_datetime

#' @import glue
#' @export
glue::glue

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
matrixStats::rowQuantiles

#' @export
iterators::icount

# #' @export
# lubridate::date

#' @importFrom remotes install_github install_gitlab install_git
#' @export
remotes::install_github

#' @export
remotes::install_gitlab

#' @export
remotes::install_git

#' @import usethis
#' @export
usethis::edit_r_environ

#' @export
usethis::edit_r_profile

#' @export
usethis::use_git_ignore

#' @export
usethis::use_build_ignore
