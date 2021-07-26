#' @export
transpose <- purrr::transpose

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
