#' melt_list
#'
#' @param list A list object, with the same colnames data.frame in every element.
#' @param var.name vector of id variables. Can be integer (variable position)
#' or string (variable name). If blank, will use all non-measured variables.
#' @param na.rm Boolean
#' @param ... other parameters to melt
#'
#' @examples
#' # data.frame
#' df <- data.frame(year = 2010, day = 1:3, month = 1, site = "A")
#' l <- list(a = df, b = df)
#' df_new <- melt_list(l, "id")
#'
#' # data.table
#' df <- data.table::data.table(year = 2010, day = 1:3, month = 1, site = "A")
#' l <- list(a = df, b = df)
#' df_new <- melt_list(l, "id")
#' @importFrom reshape2 melt
#' @importFrom data.table is.data.table
#' @export
melt_list <- function(list, var.name = "variable", na.rm = TRUE, ...) {
    if (is.null(names(list))) names(list) <- seq_along(list)

    list <- rm_empty(list)
    if (is.null(list) || length(list) == 0) {
        return(NULL)
    }
    
    first <- list[[1]]
    if (is.data.table(first)) {
        names <- names(list)
        for (i in seq_along(list)) {
            x <- list[[i]]
            eval(parse(text = sprintf("x$%s <- names[i]", var.name)))
            list[[i]] <- x
        }
        res <- do.call(rbind, list) # return
    } else {
        id.vars <- colnames(first)
        res <- reshape2::melt(list, ..., id.vars = id.vars, na.rm = na.rm)
        colnames(res) <- c(id.vars, var.name)
    }
    reorder_name(res, var.name)
}

listk <- function (...) 
{
    cols <- as.list(substitute(list(...)))[-1]
    vars <- names(cols)
    Id_noname <- if (is.null(vars)) 
        seq_along(cols)
    else which(vars == "")
    if (length(Id_noname) > 0) 
        vars[Id_noname] <- sapply(cols[Id_noname], deparse)
    x <- setNames(list(...), vars)
    return(x)
}

#' @rdname melt_list
#' @export
melt_tree <- function(x, names, ...) {
    first <- x[[1]]
    if (is.data.frame(first)) {
        if (length(names) > 1) {
              stop("length of `names` is not equal to the deep of list!")
          }
        melt_list(rm_empty(x), names[1], ...)
    } else if (is.list(first)) {
        # n <- length(names) # deeps of list
        # names2 <- names[1:(n-1)]
        map(x, melt_tree, names[-1]) %>% melt_list(names[1])
    } else {
        stop("Elements of x should be data.frame or list!")
    }
}

#' rm_empty
#' @param x A vector or list
#' 
#' @examples
#' # numeric
#' x <- c(1:5, NA)
#' rm_empty(x)
#' 
#' # list
#' l <- list(1:5, NULL, NA)
#' rm_empty(l)
#' 
#' @keywords internal
#' @rdname tools
#' @export
rm_empty <- function(x){
    if (is.list(x)){
        x[!sapply(x, is_empty)]
    }else {
        x[!is.na(x)]
    }
}

is_empty <- function(x) {
    is.null(x) || (is.data.frame(x) && nrow(x) == 0)
    # (is.numeric(x) && is.na(x))
}
