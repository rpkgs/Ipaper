#' @title data.frame manipulating function by `dplyr::across`
#' @name dt_tools
NULL

# ' @importFrom tidyselect where
#' @importFrom dplyr mutate across
#' @rdname dt_tools
#' @export
dt_round <- function(d, digits = 2) {
    mutate(d, across(where(is.double), ~ round(.x, digits)))
}

#' @rdname dt_tools
#' @export
dt_chr2num <- function(d) {
    mutate(d, across(where(is.character), ~ as.numeric(.x)))
}

# https://stackoverflow.com/questions/54774280/plyrddply-equivalent-in-dplyr


#' @importFrom data.table dcast
#' @export
dcast2 <- function(d, by, value.var = "value", ...) {
    vars_left <- setdiff(colnames(d), c(by, value.var)) %>% paste(collapse = "+")
    vars_right <- by %>% paste(collapse = "+")
    formula <- as.formula(sprintf("%s~%s", vars_left, vars_right))
    dcast(d, formula, value.var = value.var, ...)
}

#' @export 
make_dt <- function(..., ncol = 3) {
    x <- list(...)
    n <- length(x)
    nrow <- floor(n / ncol)
    lapply(1:nrow, function(i) {
        ind <- seq((i - 1) * ncol + 1, i * ncol)
        x[ind] %>% as.data.table()
    }) %>% do.call(rbind, .)
}
