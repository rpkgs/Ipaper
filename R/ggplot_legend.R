#' @export
ggplot_legend<-function(g){
    tmp <- ggplot_gtable(ggplot_build(g))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}
