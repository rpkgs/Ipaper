#' label_tag
#'
#' @param labels character vector or expression vector
#' @param tag boolean
#'
#' @examples
#' label_tag(1:5)
#' char2expr(1:5)
#' @export
label_tag <- function(labels, tag = TRUE) {
    n <- length(labels)
    tags = c(letters, LETTERS)
    sapply(seq_along(labels), function(i) {
        name = labels[[i]]
        data <- list(tag = tags[i], x = name)
        if (tag) {
            eval(substitute(expression(bold("(" * tag * ")" ~ x)), data))
            # eval(substitute(expression(bold(tag * ". " ~ x)), data))
        } else {
            eval(substitute(expression(bold(x)), data))
        }
    })
    # sprintf("(%s) %s", letters[1:n], labels)
}


#' @rdname label_tag
#' @export
char2expr <- function(labels) {
    sapply(labels, function(name) {
        eval(substitute(expression(bold(x)), list(x = name)))
    })
}

#' generate R script of character vector
#'
#' @param x character vector, data.frame or list.
#' @param collapse an optional character string to separate the results. Not NA_character_.
#'
#' @export
char2script <- function(x, collapse = '"') {
    if (is.list(x)) {
        x <- names(x)
    }

    head <- sprintf("c(%s", collapse)
    tail <- sprintf("%s)", collapse)
    collapse <- sprintf("%s, %s", collapse, collapse)

    script <- paste(x, collapse = collapse) %>% paste0(head, ., tail)

    if (.Platform$OS.type == "windows") writeLines(script, "clipboard")
    cat(script)
}

#' @export
#' @rdname char2script
code_ChrVec <- char2script
