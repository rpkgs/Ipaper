#' melt_list
#' 
#' @rdname listk
#' @param list A list object, with the same colnames data.frame in every element.
#' @param var.name vector of id variables. Can be integer (variable position) 
#' or string (variable name). If blank, will use all non-measured variables.
#' @param na.rm Boolean
#' @param ... other parameters to melt
#' 
#' @importFrom reshape2 melt
#' @importFrom data.table is.data.table
#' @export
melt_list <- function(list, var.name, na.rm = TRUE, ...){
    if (is.data.table(list[[1]])){
        names <- names(list)
        for (i in seq_along(list)){
            x <- list[[i]]
            eval(parse(text = sprintf("x$%s <- names[i]", var.name)))
            list[[i]] <- x
        }
        res <- do.call(rbind, list)#return
    } else{
        id.vars <- colnames(list[[1]])
        res <- reshape2::melt(list, ..., id.vars = id.vars, na.rm = na.rm)
        colnames(res) <- c(id.vars, var.name)
    }
    return(res)
}