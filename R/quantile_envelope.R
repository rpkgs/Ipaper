#' quantile_envelope
#' @export
quantile_envelope <- function(x, alpha){
    names <- "ymean"
    # if (alpha != 0.5) {
        alpha <- c(alpha, 1-alpha)
        names <- c("ymin", "ymax")
    # }
    res <- quantile(x, alpha, na.rm = T)
    set_names(res, names)
}
