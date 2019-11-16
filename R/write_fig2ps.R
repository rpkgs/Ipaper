#' write_fig2ps
#' 
#' Write figure to pages
#' Subplots xlab and ylab are unified, and only keet singe one.
#' Currently, only support ggplot figures; And only support arrange figures
#' into rows (nrow*1).
#'
#' @param ps A list of ggplot figure objects. And
#' @param lgd A grid grob object, legend to show in the bottom.
#' @param ylab.right y label title
#' @param width inch
#' @param height inch
#' @param nrow
#' 
#' @export
write_fig2ps <- function(ps, lgd, ylab.right, file, width = 10, height, nrow = 6){
    npage <- ceiling(length(ps)/nrow)
    if (missing(height)) height = nrow*1.6

    ylab.left        <- ps[[1]]$labels$y
    ylab.left.color  <- ps[[1]]$theme$axis.title.y.left$colour
    ylab.right.color <- ps[[1]]$theme$axis.title.y.right$colour

    params <- list(ncol = 1, padding = unit(1, "line"),
        left  = textGrob(ylab.left , rot = 90,
                         gp=gpar(fontsize=14, col=ylab.left.color)) )

    # parameters for arrangeGrob
    if (!missing(ylab.right))
        params$right = textGrob(ylab.right, rot = 270,
                                gp=gpar(fontsize=14, col=ylab.right.color))

    Cairo::CairoPDF(file, width, height)
    for (i in 1:npage){
        runningId(i)
        I_beg <- (i - 1) * nrow + 1
        I_end <- min(i*nrow, length(ps))

        I  <- I_beg:I_end
        n  <- length(I)

        ps_i <- ps[I]
        for (j in seq_along(I)){
            theme_j <- theme(
                axis.text.x = element_blank(),
                axis.title = element_blank(),
                axis.title.y.right = element_blank(),
                axis.title.y.left  = element_blank(), 
                legend.position="none"
            )
            if (j == n)
                theme_j <- theme(
                    axis.title.y.right = element_blank(),
                    axis.title.y.left  = element_blank(), 
                    legend.position="none" )
            ps_i[[j]] <- ps_i[[j]] + theme_j
        }

        ps_i  <- c(ps_i, list(lgd))
        nx    <- length(ps_i)

        params$grobs <- ps_i
        params$nrow  <- nx

        if (missing(lgd)){
            params$heights <- c(rep(5, nx - 1), 5.5)
        } else{
            params$heights <- c(rep(5, nx - 2), 5.5, 2)
        }

        g <- do.call(gridExtra::arrangeGrob, params)

        if (i != 1) grid.newpage();
        grid::grid.draw(g)
    }
    dev.off()
    file.show(file)
}

