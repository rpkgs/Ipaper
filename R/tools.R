#' fprintf
#' Print sprintf result into console, just like C style fprintf function
#' @param fmt a character vector of format strings, each of up to 8192 bytes.
#' @param ... other parameters will be passed to `sprintf`
#'
#' @examples
#' cat(fprintf("%s\n", "Hello phenofit!"))
#' @export
fprintf <- function(fmt, ...) cat(sprintf(fmt, ...))

#' print the running ID in the console
#' 
#' @param i the running Id.
#' @param N The number of total iteration 
#' @param step how long of print step.
#' @param prefix prefix string
#' 
#' @examples
#' for (i in 1:10){
#'     runningId(i, prefix = "phenofit")
#' }
#' @export
runningId <- function(i, step = 1, N, prefix = "") {
    perc <- ifelse(missing(N), "", sprintf(", %.1f%% ", i/N*100))
    if (mod(i, step) == 0) cat(sprintf("[%s] running%s %d ...\n", prefix, perc, i))    
}

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

#' @export
mark_outlier <- function(x, nsd = 3) {
    sd <- sd(x, na.rm = TRUE)
    mean <- mean(x, na.rm = TRUE)
    max <- mean + nsd * sd
    min <- mean - nsd * sd
    x[x > max | x < min] <- NA_real_
    x
}

#' @export
unique_length <- function(x) {
    unique(x) %>% length()
}

#' @export
match2 <- function(x, y) {
    I <- match(x, y)
    I_x <- which.notna(I)
    I_y <- I[I_x]

    d <- data.table(
        x = x[I_x], y = y[I_y], I_x, I_y,
        grp = cumsum(c(TRUE, diff(I_y) != 1))
    )
    d
}

# #' @export
# first <- function(x) {
#     x[[1]]
# }

# #' @export
# last <- function(x) {
#     x[[length(x)]]
# }
