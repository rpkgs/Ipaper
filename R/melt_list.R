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
#' 
#' melt_list(l, 'id') %>% as_tibble()
#' melt_list(l, 'id' = 1) %>% as_tibble()
#' melt_list(set_names(l, NULL), 'id') %>% as_tibble()
#' 
#' # data.table
#' df <- data.table::data.table(year = 2010, day = 1:3, month = 1, site = "A")
#' l <- list(a = df, b = df)
#' df_new <- melt_list(l, "id")
#' @importFrom data.table melt
#' @importFrom data.table is.data.table
#' @export
melt_list <- function(list, ..., na.rm = TRUE) {
    list <- rm_empty(list)
    if (is.null(list) || length(list) == 0) {
        return(NULL)
    }
    n <- length(list)

    params = list(...)
    key = names(params)[1]
    vals = params[[1]]
    if (is.null(key)) {
        key = vals; # variable name
        vals = names(list)
    }
    if (is.null(vals)) vals <- seq_along(list)
    if (length(vals) == 1) vals = rep(vals, n)
    if (is.character(vals)) vals %<>% as.factor()

    first <- list[[1]]
    if (is.data.frame(first)) {    
        for (i in seq_along(list)) {
            x <- list[[i]]
            eval(parse(text = sprintf("x$%s <- vals[i]", key)))
            list[[i]] <- x
        }
        res <- do.call(rbind, list) %>% data.table() # return
    # } else {
    #     id.vars <- colnames(first)
    #     res <- data.table::melt(list, ..., id.vars = id.vars, na.rm = na.rm)
    #     colnames(res) <- c(id.vars, key)
    }
    res %>% dplyr::relocate(key)
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

#' @export
is_empty <- function(x) {
    is.null(x) || (is.data.frame(x) && nrow(x) == 0) || length(x) == 0
    # (is.numeric(x) && is.na(x))
}

empty <- is_empty
