#' contain
#' find assigned pattern variable names
#' @param d A data.frame vector, or list
#' @param pattern string used to match `names(d)`
#' 
#' @examples
#' df <- data.frame(year = 2010, day = 1:3, month = 1, site = "A")
#' contain(df, "year|month|day")
#' @rdname tools
#' @export
contain <- function(d, pattern = "NDVI|EVI") {
    names(d) %>% .[grep(pattern, .)]
}
