#' @export
stat_sd <- function(x, digit = 2, ...){
    x <- x[!is.na(x)]
    y <- mean(x)
    ymedian = median(x)
    sd <- sd(x)
    fmt <- sprintf("%%.%df±%%.%df", digit, digit)
    label <- sprintf(fmt, ymedian, sd)
    list(y = y, ymin = y-sd, ymax = y+sd, ymedian, sd = sd, label = label)
}

# 25% and 75% quantile
#' @export
stat_quantile2 <- function(x, probs = c(0.25, 0.75), ...){
    # browser()
    x <- x[!is.na(x)]
    y <- median(x)
    # sd <- sd(x)
    r <- quantile(x, probs = probs, na.rm = TRUE)
    c(y = y, ymin = r[[1]], ymax = r[[2]])
}

#' @export
box_qtl <- function(x){
    x <- stats::na.omit(x)
    quantile(x, c(0.1, 0.9)) %>% set_names(c("ymin", "ymax"))
}

geom_quantile2 <- function(){
    # browser()
    p_attribute <- ggplot(d[variable != "EOS"], aes(variable, perc, fill = variable)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2(outlier.size = -1) + 
        stat_summary(fun.data = FUN_lab, colour = "black", size = fontsize_statistic, geom = "text", vjust = -0.5) +
        labs(y = "Relative contribution to EOS change (%)", x = NULL)
}

# ggplot 打开了新大门
# stat_summary(fun.data = stat_quantile, colour = "black", size = 1, geom = "errorbar")
