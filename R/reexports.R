#' @export
transpose <- purrr::transpose

#' @importFrom plyr mapvalues revalue
#' @export
plyr::mapvalues

#' @export
plyr::revalue

#' @importFrom dplyr mutate top_n top_frac as_tibble tribble select rename arrange
# #' @export
# dplyr::first

# #' @export
# dplyr::last

first <- function(x) {
  x[1]
}

last <- function(x) {
  x[length(x)]
}


#' @export
dplyr::select

#' @export
dplyr::rename

#' @export
dplyr::arrange

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


# #' @importFrom pillar dim_desc 
#' @export
print.data.table <- function(d, n = 10, ..., maxrows = 1e6) {
  # cat(sprintf("[data.table]: %s \n", dim_desc(d)))
  if (nrow(d) <= maxrows) {
    cat("[data.table]: \n")
    
    class(d) <- c("tbl", "data.table", "data.frame")
    print(d, n = n, ...)
  } else {
    data.table:::print.data.table(d)
  }
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

#' @import zeallot
#' @export
zeallot::`%->%`

#' @export
zeallot::`%<-%`
