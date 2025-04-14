str_extract <- function(x, pattern) {
    ans <- gregexpr(pattern, x, perl = TRUE)[[1]]
    substr(x, ans, ans + attr(ans, "match.length"))
}

str_locate_all.vec <- function(x, pattern) {
    ans <- gregexpr(pattern, x, perl = TRUE)[[1]]
    ans
}

str_locate_all <- function(x, pattern) {
    ans <- gregexpr(pattern, x, perl = TRUE)[[1]]
    data.table(I = seq_along(ans), start = ans, end = attr(ans, "match.length") + ans - 1)
}

#' @export
str_year <- function(x) str_extract(basename(x), "\\d{4}")

#' @export
#' @rdname file_name
file_ext <- function(file) {
    ext = str_extract(basename(file), "(?<=\\.).{1,4}$") 
    if (grepl(" ", ext)) ext = ""
    ext
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
    pos   <- str_locate_all.vec(file, "\\.")
    pos   <- pos[[length(pos)]]
    # I_dot <- pos[nrow(pos), 1]
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

#' obj_size
#'
#' Get object size in `unit`
#' @param obj Object
#' @param unit "Kb", "Mb" or "Gb"
#'
#' @examples
#' obj_size(1:100)
#' @export
obj_size <- function(obj, unit = "Mb") {
    cat(format(object.size(obj), unit), "\n")
}

#' @rdname obj_size
#' @export
obj.size <- obj_size

#' file_size
#'
#' @param file file path
#' @export
file_size <- function(file) {
    utils:::format.object_size(file.size(file), "auto")
}

#' dir2
#' 
#' @inheritParams base::dir
#' @param ... other parameters to [base::dir()]
#' 
#' @seealso [base::dir()]
#' @export
dir2 <- function(path = ".", pattern = NULL, full.names = TRUE, ...) {
    dir(path_mnt(path), pattern, ..., full.names = full.names)
}

#' @export
isfile <- function(f) {
  file.exists(file)
}
