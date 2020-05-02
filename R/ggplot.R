#' @importFrom ggplot2 ggplot_gtable
#' @export
ggplot_legend<-function(g){
    tmp <- ggplot_gtable(ggplot_build(g))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

#' @importFrom ggplot2 ggplot_build geom_text aes_string 
#' @export
facet_tag <- function (p, open = "(", close = ")", tag_pool = letters, x = -Inf, 
    I_start = 1, 
    update_theme = FALSE,
    y = Inf, hjust = -0.5, vjust = 1.5, fontface = 2, fontsize = 14, family = "TimesSimSun", 
    ...) 
{
    gb <- ggplot_build(p)
    lay <- gb$layout$layout
    tags <- cbind(lay, label = paste0(open, tag_pool[as.numeric(lay$PANEL) + I_start - 1], 
                                      close), x = x, y = y)
    p <- p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), 
                  ..., hjust = hjust, vjust = vjust, fontface = fontface, size = fontsize, 
                  family = family, inherit.aes = FALSE)
    
    # update_theme = TRUE
    if (update_theme) {
        p <- p +
            theme(
                panel.background = element_rect(fill = "white"),
                panel.border = element_rect(fill = NA, color = "grey60", size = 0.5),
                strip.text = element_text(size = fontsize - 1, family = family),
                # strip.text = element_blank(),
                strip.background = element_blank(), 
                axis.title = element_text(size = fontsize, face = 2),
                legend.title = element_text(size = fontsize+1, face = 2),
                legend.text = element_text(size = fontsize+1)
            )
    }
    p
}

# theme(
#     legend.position = "bottom",
#     axis.text = element_text(color = "black", size = 17),
#     plot.margin = margin(-5, 0, 0, 0),
#     legend.box.margin = margin(-5, 0, 0, 0),
#     legend.text = element_text(size = 18),
#     axis.title.x = element_text(margin = margin(0.2, 0, -0.3, 0, "cm"))
# )

#' @export
label_tag <- function(labels, tag = TRUE) {
    n <- length(labels)
    foreach(name = labels, i = icount(), .combine = c) %do% {
        data <- list(tag = letters[i], x = name)
        if (tag) {
            eval(substitute(expression(bold("(" * tag * ")" ~ x)), data))
        } else {
            eval(substitute(expression(bold(x)), data))
        }
    }
    # sprintf("(%s) %s", letters[1:n], labels)
}

#' @export
char2expr <- function(names) {
    foreach(name = names, .combine = c) %do% {
        eval(substitute(expression(bold(x)), list(x = name)))
    }
}

#' generate R script of character vector
#'
#' @param x character vector, data.frame or list.
#' @param collapse an optional character string to separate the results. Not NA_character_.
#'
#' @export
char2script <- function(x, collapse = '"') {
    if (is.list(x)) {
        x <- names(x)
    }

    head <- sprintf("c(%s", collapse)
    tail <- sprintf("%s)", collapse)
    collapse <- sprintf("%s, %s", collapse, collapse)

    script <- paste(x, collapse = collapse) %>% paste0(head, ., tail)

    if (.Platform$OS.type == "windows") writeLines(script, "clipboard")
    cat(script)
}

#' @export
#' @rdname char2script
code_ChrVec <- char2script
