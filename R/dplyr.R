#' @export
transpose <- purrr::transpose

# ' @importFrom tidyselect where
#' @importFrom dplyr mutate across
#' @export
dt_round <- function(d, digits = 2) {
    mutate(d, across(where(is.double), ~ round(.x, digits)))
}

# https://stackoverflow.com/questions/54774280/plyrddply-equivalent-in-dplyr
