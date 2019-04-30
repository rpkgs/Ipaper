#' @export
tidy_file <- function(file) {
    gsub("file:///", "", file)
}
