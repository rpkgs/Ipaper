#' chunk
#'
#' @references https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
#' @export
chunk <- function(x, nchunk = 6) {
    split(x, cut(seq_along(x), nchunk, labels = FALSE)) %>% set_names(NULL)
}

#' @export
split_data <- function(x, nchunk = 6, byrow = TRUE) {
    n <- length(x)
    if (is.matrix(x)) n <- ifelse(byrow, nrow(x), ncol(x))

    lst_inds <- chunk(1:n, nchunk)
    if (is.matrix(x)) {
        FUN <- if (byrow) . %>% x[., ] else . %>% x[, ., drop = FALSE]
        lapply(lst_inds, FUN)
    } else if (is.list(x) || is.vector(x)) {
        lapply(lst_inds, . %>% x[.])
    } else {
        stop("unsupported type!")
    }
}

#' @importFrom parallel parLapply
#' @export
llply_par <- function(X, FUN, ..., byrow = TRUE) {
    nchunk <- length(getOption("cl"))
    lst <- split_data(X, nchunk, byrow = byrow)
    parLapply(getOption("cl"), lst, FUN, ...) %>% do.call(c, .)
}

#' @export
parLapply2 <- llply_par

#' @export
apply_par <- function(X, .margins = 1, FUN, ..., .progress = "text") {
    byrow = .margins == 1
    res = parLapply2(X, function(x) {
        plyr::aaply(x, .margins, FUN, ..., .progress = .progress)
    }, byrow = byrow)

    comb = if (is.matrix(res[[1]])) rbind else c
    do.call(comb, res)
}

#' @export
check_cluster <- function(ncluster = 6) {
    if (length(getOption("cl")) == 0) InitCluster(ncluster)
}
