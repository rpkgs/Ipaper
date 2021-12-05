# add progress at here

# pb <- progress_bar$new(
#     total = n,
#     format = "[:bar] :current/:total (:percent) eta: :eta"
# )
# pb$tick()

# https://github.com/TylerGrantSmith/purrrgress

# , .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL
#' @importFrom purrr as_mapper
#' @export
llply <- function(.data, .f = NULL, .progress = "none", ...) { 
    if (is_empty(.data)) return(.data)
    n = length(.data)
    .f <- as_mapper(.f, ...)
    lapply(.data, .f)
}

#' @export
ldply <- function(.data, .f = NULL, ...) {
    llply(.data, .f, ...) %>% as.data.table()
}
