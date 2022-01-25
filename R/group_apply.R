#' group_apply
#' 
#' @param brks a numeric vector
#' @param nchunk split data into n chunks by setting `brks = pretty(x, nchunk)`, 
#' and apply `FUN` in every group. If `brks` provided, `step` will be ignored.
#' @param step split data into chunks byn setting `brks = seq(xrange[1], xrange[2], by = step)`.
#' If `brks` or `nchunk` provided, `step` will be ignored.
#' 
#' @param FUN function of mean or median
#' @param ... others to `FUN`
#' 
#' @export
group_apply <- function(x, y, FUN = "mean", 
                        nchunk=20, step = 0.2, brks = NULL, ...) 
{
    if (is.character(FUN)) FUN <- get(FUN, mode="function")

    if (is.null(brks)) {
        if (!is.null(nchunk)) {
            brks = pretty(x, nchunk)
            # brks <- x2[seq(1, N, nchunk)]
        } else if (!is.null(step)) {
            xrange <- range(x, na.rm = T)
            brks = seq(xrange[1], xrange[2], by = step)
        }
    }
    
    n    <- length(brks)
    xmid <- (brks[-n] + brks[-1])/2
    brks[n] <- Inf

    res = foreach(i = 1:(n-1)) %do% {
        val_min <- brks[i]
        val_max <- brks[i + 1]

        I <- x >= val_min & x < val_max
        FUN(y[I], ..., na.rm = T)
    } %>% do.call(rbind, .) %>% as.data.table()
    cbind(x = xmid, res)
}


#' @keywords internal
#' @rdname group_apply
#' @export
upper_envelope <- function(x, y, interval = c(.50, .80, .90, .95), nchunk = 50) {
        
    d_prob = data.table(I = seq_along(interval),
                        interval = sprintf("%d%%", interval*100) %>% {factor(., rev(.))},
                        lower = (1 - interval)/2, upper = 1 - (1 - interval)/2, mid = 0.5)

    rows = (1:nrow(d_prob)) %>% set_names(., .)
    d = foreach(i = rows) %do% {
        prob = d_prob[i, .(lower, upper, mid)] %>% as.numeric()
        ans = group_apply(x, y, quantile, probs = prob, nchunk = nchunk)

        set_colnames(ans, c("x", "lower", "upper", "mid")) %>%
            cbind(I = i, interval = d_prob$interval[i], .)
    } %>% do.call(rbind, .)
    d
}
