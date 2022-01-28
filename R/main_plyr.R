# add progress at here

# pb <- progress_bar$new(
#     total = n,
#     format = "[:bar] :current/:total (:percent) eta: :eta"
# )
# pb$tick()

#' plyr function in purrr style
#' 
#' @inheritParams plyr::llply
#' @param ... other arguments passed on to `.f`
#' 
#' @references
#' 1. <https://github.com/TylerGrantSmith/purrrgress>
#' 
#' @examples
#' x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
#' llply(x, mean, .progress = "text")
#' llply(x, ~mean(.x), .progress = TRUE)
#' llply(x, quantile, probs = 1:3 / 4)
#' 
#' @importFrom purrr as_mapper
#' @importFrom foreach %dopar% %do%
#' @export
llply <- function(.data, .f = NULL, .progress = "none", .parallel = FALSE, ...) { 
    if (is_empty(.data)) return(.data)
    n = length(.data)
    
    `%dof%` = ifelse(.parallel, `%dopar%`, `%do%`)
    if (.parallel) .progress = FALSE

    if (isTRUE(.progress) || .progress == "text") {
        pro_map(.data, .f, ...)
    } else {
        .f <- as_mapper(.f, ...)
        foreach(x = .data) %dof% {
            .f(x)
        }
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
