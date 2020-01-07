# #' @export
# first <- function(x) {
#     x[[1]]
# }

# #' @export
# last <- function(x) {
#     x[[length(x)]]
# }

#' @export
tabular <- function(df, ...) {
    stopifnot(is.data.frame(df))

    align <- function(x) if (is.numeric(x)) "r" else "l"
    col_align <- vapply(df, align, character(1))

    cols <- lapply(df, format, ...)
    contents <- do.call("paste",
        c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))
    paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
        contents, "\n}\n", sep = "")
}


# for levelplot2
parse.formula <- function(formula = x~s1+s2) {
    str_formula <- gsub("s1 \\+ s2 *\\|*| ", "", as.character(formula))
    value.var = str_formula[2]
    groups    = strsplit(str_formula[3], "\\+|\\*")[[1]]
    list(value.var = value.var, groups = groups)    
}

#' generate R script of character vector
#' 
#' @param x character vector, data.frame or list.
#' @inheritParams base::paste
#' 
#' @export
code_ChrVec <- function(x, collapse = '"') {
    if(is.list(x)) {
        x = names(x)
    }
    
    head = sprintf('c(%s', collapse)
    tail = sprintf('%s)', collapse)
    collapse = sprintf('%s, %s', collapse, collapse) 
    
    script = paste(x, collapse = collapse) %>% paste0(head, ., tail)

    if (.Platform$OS.type == "windows") writeLines(script, "clipboard")
    cat(script)
}

#' obj.size
#'
#' Get object size in `unit`
#' @param obj Object
#' @param unit "Kb", "Mb" or "Gb"
#'
#' @examples
#' obj.size(1:100)
#' @export
obj.size <- function(obj, unit = "Mb") {
    cat(format(object.size(obj), unit), "\n")
}

#' file_size
#'
#' @param file file path
#' @export
file_size <- function(file) {
    utils:::format.object_size(file.size(file), "auto")
}

#' ifelse2
#'
#' ternary operator just like java `test ? yes : no`
#'
#' @param test an object which can be coerced to logical mode.
#' @param yes return values for true elements of test.
#' @param no return values for false elements of test.
#'
#' @examples
#' x <- ifelse2(TRUE, 1:4, 1:10)
#' @export
ifelse2 <- function(test, yes, no) {
    if (test) yes else no
}

#' fprintf
#' Print sprintf result into console, just like C style fprintf function
#' @param fmt a character vector of format strings, each of up to 8192 bytes.
#' @param ... other parameters will be passed to `sprintf`
#'
#' @examples
#' cat(fprintf("%s\n", "Hello phenofit!"))
#' @export
fprintf <- function(fmt, ...) cat(sprintf(fmt, ...))

#' @export
which.na <- function(x) {
    which(is.na(x))
}

#' @export
which.notna <- function(x) {
    which(!is.na(x))
}
