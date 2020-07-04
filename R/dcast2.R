#' @export
dcast2 <- function(d, by, value.var = "value", ...){
    vars_left <- setdiff(colnames(d), c(by, value.var)) %>% paste(collapse = "+")
    vars_right <- by %>% paste(collapse = "+")
    formula <- as.formula(sprintf("%s~%s", vars_left, vars_right))
    dcast(d, formula, value.var = value.var, ...)
}
