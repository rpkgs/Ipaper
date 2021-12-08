# add progress at here

# pb <- progress_bar$new(
#     total = n,
#     format = "[:bar] :current/:total (:percent) eta: :eta"
# )
# pb$tick()

#' plyr function in purrr style
#' 
#' @importFrom purrr as_mapper
#' 
#' @references
#' 1. <https://github.com/TylerGrantSmith/purrrgress>
#' 
#' @examples
#' x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
#' llply(x, mean, .progress = "text")
#' llply(x, ~mean(.x), .progress = T
#' 
#' llply(x, quantile, probs = 1:3 / 4)
#' @export
llply <- function(.data, .f = NULL, .progress = "none", ...) { 
    if (is_empty(.data)) return(.data)
    n = length(.data)
    if (isTRUE(.progress) || .progress == "text") {
        pro_map(.data, .f, ...)
    } else {
        .f <- as_mapper(.f, ...)
        lapply(.data, .f)
    }
}

#' @rdname llply
#' @export
ldply <- function(.data, .f = NULL, ...) {
    llply(.data, .f, ...) %>% as.data.table()
}

#' @rdname llply
#' @export
laply <- function(.data, .f = NULL, ...) {
    llply(.data, .f, ...) %>% unlist()
}

#' @rdname llply
#' @export
map_simplify <- laply
