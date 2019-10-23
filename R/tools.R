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
