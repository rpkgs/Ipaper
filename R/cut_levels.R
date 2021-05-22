#' cut_levels
#' 
#' @param x numeric vector
#' @param pvalue p <= `x%`, means its significant at `x%` level
#' 
#' @examples
#' x <- c(-0.09, -0.4, 0.04, 0.15)
#' cut_levels(x, verbose = TRUE)
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

#' @export
mapvalues <- function (x, from, to, warn_missing = TRUE) {
    if (length(from) != length(to)) {
        stop("`from` and `to` vectors are not the same length.")
    }
    if (!is.atomic(x)) {
        stop("`x` must be an atomic vector.")
    }
    if (is.factor(x)) {
        levels(x) <- mapvalues(levels(x), from, to, warn_missing)
        return(x)
    }
    mapidx <- match(x, from)
    mapidxNA <- is.na(mapidx)
    from_found <- sort(unique(mapidx))
    if (warn_missing && length(from_found) != length(from)) {
        message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
    }
    x[!mapidxNA] <- to[mapidx[!mapidxNA]]
    x
}
