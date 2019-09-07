#' cut_levels
#' 
#' @param x numeric vector
#' @param pvalue p <= `x%`, means its significant at `x%` level
#' 
#' @examples
#' x <- c(-0.09, -0.4, 0.04, 0.15)
#' cut_levels(x, verbose = TRUE)
#' @importFrom plyr mapvalues
#' @export
cut_levels <- function(x, pvalue = c(0.01, 0.05, 0.1), verbose = FALSE){
    np <- length(pvalue) + 1
    pvalue2 <- pvalue %>% c(., 1) %>% c(-rev(.), 0, .)

    levels_num <- cut(1, pvalue2) %>% levels() %>% {
        c(rev(.[1:np]), rev(.[-(1:np)])) %>% rev()
    }
    levels_str <- c(
        sprintf("significant increasing at the %-4s level", as.character(pvalue)), 
        "insignificant increasing", 
        "insignificant decreasing", 
        sprintf("significant decreasing at the %-4s level", rev(as.character(pvalue)))) 
    levels <- cbind(levels_num, levels_str)
    if (verbose) print(levels)
    
    xf <- cut(x, pvalue2) %>% factor(levels_num, levels_str)
    xf
}
