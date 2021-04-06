str_extract <- function(x, pattern) {
    ans <- gregexpr(pattern, x, perl = TRUE)[[1]]
    substr(x, ans, ans + attr(ans, "match.length"))
}

str_locate_all <- function(x, pattern) {
    ans <- gregexpr(pattern, x, perl = TRUE)[[1]]
    ans
}

#' @export
#' @rdname file_name
file_ext <- function(file) {
    str_extract(basename(file), "(?<=\\.).{1,4}$") 
}

#' get file name and extension
#' 
#' @param file file path
#' 
#' @examples 
#' file_name("./a.pdf")
#' file_ext("./a.pdf")
#' @export
file_name <- function(file) {
    file  <- basename(file)
    pos   <- str_locate_all(file, "\\.")
    # browser()
    pos   <- pos[[length(pos)]]
    # I_dot <- pos[nrow(pos), 1]
    # browser()

    # no dot
    if (pos < 0) return(file)
    
    postfix = substr(file, pos, nchar(file))
    if (nchar(postfix) <= 5) {
        gsub(postfix, "", file)
    } else {
        file
    }
    # gsub("\\..*", "", basename(file))
    # str_extract(basename(file), ".*(?=\\.?)")
    # ?: zero or one
    # *: zero or more
    # +:  one or more
}
