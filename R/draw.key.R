process.key <-
    function(reverse.rows = FALSE,
             between = 2,
             align = TRUE,
             title = NULL,
             rep = TRUE,
             background = trellis.par.get("background")$col,
             alpha.background = 1,
             border = FALSE,
             transparent = FALSE,
             col = "black",
             alpha = 1,
             lty = 1,
             lwd = 1,
             font = 1,
             fontface = NULL,
             fontfamily = NULL,
             pch = 8,
             cex = 1,
             fill = "transparent",
             adj = 0,
             type = "l",
             size = 5,
             height = 1,
             angle = 0,
             density = -1,
             cex.title = 1.5 * max(cex),
             padding.text = 1,
             lineheight = 1,
             columns = 1,
             divide = 3,
             between.columns = 3,
             ...,
             lines.title = 2) {
        list(
            reverse.rows = reverse.rows,
            between = between,
            align = align,
            title = title,
            rep = rep,
            background = background,
            alpha.background = alpha.background,
            border = border,
            transparent = transparent,
            col = col,
            alpha = alpha,
            lty = lty,
            lwd = lwd,
            font = font,
            fontface = fontface,
            fontfamily = fontfamily,
            pch = pch,
            cex = cex,
            fill = fill,
            adj = adj,
            type = type,
            size = size,
            height = height,
            angle = angle,
            density = density,
            cex.title = cex.title,
            padding.text = padding.text,
            lineheight = lineheight,
            columns = columns,
            divide = divide,
            between.columns = between.columns,
            lines.title = lines.title,
            ...
        )
    }

#' @export
draw.key <- function(key, draw = FALSE, vp = NULL, ...) {
    if (!is.list(key))
        stop("key must be a list")
    max.length <- 0

    fontsize.points <- trellis.par.get("fontsize")$points
    key <- do.call(process.key, key, quote = TRUE)
    key.length <- length(key)
    key.names <- names(key)
    if (is.logical(key$border))
        key$border <- if (key$border)
            "black" else "transparent"
    components <- list()
    for (i in 1:key.length) {
        curname <- pmatch(key.names[i], c("text", "rectangles", "lines", "points"))
        if (is.na(curname)) {
        } else if (curname == 1) {
            if (!(is.characterOrExpression(key[[i]][[1]])))
                stop("first component of text must be vector of labels")
            pars <- list(labels = key[[i]][[1]], col = key$col,
                         alpha = key$alpha, adj = key$adj, cex = key$cex,
                         lineheight = key$lineheight, font = key$font,
                         fontface = key$fontface, fontfamily = key$fontfamily)
            pars <- pars[!sapply(pars, is.null)]
            key[[i]][[1]] <- NULL
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- length(pars$labels)
            for (j in 1:length(pars)) if (is.character(pars))
                pars[[j]] <- rep(pars[[j]], length.out = tmplen)
            max.length <- max(max.length, tmplen)
            components[[length(components) + 1]] <-
                list(type = "text", pars = pars, length = tmplen)
        } else if (curname == 2) {
            pars <- list(col = key$col, border = "black", alpha = key$alpha,
                         size = key$size, height = key$height, angle = key$angle,
                         density = key$density)
            pars <- pars[!sapply(pars, is.null)]
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars, length)))
            max.length <- max(max.length, tmplen)
            components[[length(components) + 1]] <- list(type = "rectangles",
                                                         pars = pars, length = tmplen)
        } else if (curname == 3) {
            pars <- list(col = key$col, alpha = key$alpha, size = key$size,
                         lty = key$lty, cex = key$cex, pch = key$pch,
                         fill = key$fill, lwd = key$lwd, type = key$type)
            pars <- pars[!sapply(pars, is.null)]
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars, length)))
            max.length <- max(max.length, tmplen)
            components[[length(components) + 1]] <- list(type = "lines",
                                                         pars = pars, length = tmplen)
        } else if (curname == 4) {
            pars <- list(col = key$col, alpha = key$alpha, cex = key$cex,
                         pch = key$pch, lwd = key$lwd, fill = key$fill,
                         font = key$font, fontface = key$fontface, fontfamily = key$fontfamily)
            pars <- pars[!sapply(pars, is.null)]
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars, length)))
            max.length <- max(max.length, tmplen)
            components[[length(components) + 1]] <- list(type = "points",
                                                         pars = pars, length = tmplen)
        }
    }
    number.of.components <- length(components)
    if (number.of.components == 0)
        stop("Invalid key, need at least one component named lines, text, rect or points")
    for (i in seq_len(number.of.components)) {
        if (key$rep && (components[[i]]$type != "text"))
            components[[i]]$length <- max.length
        components[[i]]$pars <- lapply(components[[i]]$pars,
                                       rep, length.out = components[[i]]$length)
        if (key$reverse.rows)
            components[[i]]$pars <- lapply(components[[i]]$pars, rev)
    }

    column.blocks <- key$columns
    rows.per.block <- ceiling(max.length/column.blocks)
    if (column.blocks > max.length)
        warning("not enough rows for columns")
    key$between <- rep(key$between, length.out = number.of.components)
    if (key$align) {
        n.row         <- rows.per.block + 1
        n.col         <- column.blocks * (1 + 3 * number.of.components) - 1
        textMatrix    <- matrix(0, n.row, n.col)
        textList      <- list()
        textCex       <- numeric(0)
        heights.x     <- rep(1, n.row)
        heights.units <- rep("lines", n.row)
        heights.data  <- vector(mode = "list", length = n.row)
        if (length(key$title) > 0) {
            stopifnot(length(key$title) == 1, is.characterOrExpression(key$title))
            heights.x[1]      <- key$lines.title * key$cex.title
            heights.units[1]  <- "strheight"
            heights.data[[1]] <- key$title
        } else heights.x[1] <- 0
        widths.x     <- rep(key$between.columns, n.col)
        widths.units <- rep("strwidth", n.col)
        widths.data  <- as.list(rep("o", n.col))
        for (i in 1:column.blocks) {
            widths.x[(1:number.of.components - 1) * 3 + 1 + (i - 1) * 3 * number.of.components + i - 1] <- key$between/2
            widths.x[(1:number.of.components - 1) * 3 + 1 + (i - 1) * 3 * number.of.components + i + 1] <- key$between/2
        }

        # browser()

        index <- 1
        for (i in 1:number.of.components) {
            cur <- components[[i]]
            id <- (1:column.blocks - 1) * (number.of.components * 3 + 1) + i * 3 - 1
            if (cur$type == "text") {
                for (j in 1:cur$length) {
                    colblck <- ceiling(j/rows.per.block)
                    xx <- (colblck - 1) * (number.of.components * 3 + 1) + i * 3 - 1
                    yy <- j%%rows.per.block + 1
                    if (yy == 1) yy <- rows.per.block + 1
                    textMatrix[yy, xx] <- index
                    textList <- c(textList, list(cur$pars$labels[j]))
                    textCex  <- c(textCex, cur$pars$cex[j])
                    index    <- index + 1
                }
            } else if (cur$type == "rectangles") {
                widths.x[id] <- max(cur$pars$size)
            } else if (cur$type == "lines") {
                widths.x[id] <- max(cur$pars$size)
            } else if (cur$type == "points") {
                widths.x[id] <- max(cur$pars$cex)
            }
        }
        heights.insertlist.position <- 0
        heights.insertlist.unit <- unit(1, "null")
        for (i in seq_len(n.row)) {
            textLocations <- textMatrix[i, ]
            if (any(textLocations > 0)) {
                textLocations <- textLocations[textLocations > 0]
                strbar <- textList[textLocations]
                heights.insertlist.position <- c(heights.insertlist.position, i)
                heights.insertlist.unit <- unit.c(heights.insertlist.unit,
                                                  unit(0.2 * key$padding.text, "lines") + max(unit(textCex[textLocations], "strheight", strbar)))
            }
        }
        layout.heights <- unit(heights.x, heights.units, data = heights.data)
        if (length(heights.insertlist.position) > 1)
            for (indx in 2:length(heights.insertlist.position))
                layout.heights <- rearrangeUnit(layout.heights,
                                                heights.insertlist.position[indx], heights.insertlist.unit[indx])
        widths.insertlist.position <- 0
        widths.insertlist.unit <- unit(1, "null")
        for (i in 1:n.col) {
            textLocations <- textMatrix[, i]
            if (any(textLocations > 0)) {
                textLocations <- textLocations[textLocations > 0]
                strbar <- textList[textLocations]
                widths.insertlist.position <- c(widths.insertlist.position, i)
                widths.insertlist.unit <- unit.c(widths.insertlist.unit,
                                                 max(unit(textCex[textLocations], "strwidth", strbar)))
            }
        }
        layout.widths <- unit(widths.x, widths.units, data = widths.data)
        if (length(widths.insertlist.position) > 1)
            for (indx in 2:length(widths.insertlist.position))
                layout.widths <- rearrangeUnit(layout.widths,
                                               widths.insertlist.position[indx], widths.insertlist.unit[indx])
        key.layout <- grid.layout(nrow = n.row, ncol = n.col,
                                  widths = layout.widths, heights = layout.heights,
                                  respect = FALSE,
                                  just = if (is.null(key$just)) "center" else key$just)
        key.gf <- frameGrob(layout = key.layout, vp = vp, name = trellis.grobname("frame", type = "key"))
        if (!key$transparent)
            key.gf <- placeGrob(key.gf, rectGrob(gp = gpar(fill = key$background,
                                                           alpha = key$alpha.background, col = key$border),
                                                 name = trellis.grobname("background", type = "key")),
                                row = NULL, col = NULL) else key.gf <- placeGrob(key.gf, rectGrob(gp = gpar(col = key$border),
                                                                                                  name = trellis.grobname("background", type = "key")),
                                                                                 row = NULL, col = NULL)
        if (!is.null(key$title))
            key.gf <- placeGrob(key.gf, textGrob(label = key$title,
                                                 gp = gpar(cex = key$cex.title, lineheight = key$lineheight),
                                                 name = trellis.grobname("title", type = "key")),
                                row = 1, col = NULL)
        for (i in 1:number.of.components) {
            cur <- components[[i]]
            for (j in seq_len(cur$length)) {
                colblck <- ceiling(j/rows.per.block)
                xx <- (colblck - 1) * (number.of.components * 3 + 1) + i * 3 - 1
                yy <- j%%rows.per.block + 1
                if (yy == 1) yy <- rows.per.block + 1
                componentx <- (colblck - 1) * (number.of.components) + i
                componenty <- (j - 1)%%rows.per.block + 1
                if (cur$type == "text") {
                    key.gf <- placeGrob(key.gf, textGrob(x = cur$pars$adj[j],
                                                         hjust = cur$pars$adj[j], label = cur$pars$labels[j],
                                                         gp = gpar(col = cur$pars$col[j], alpha = cur$pars$alpha[j],
                                                                   lineheight = cur$pars$lineheight[j], fontfamily = cur$pars$fontfamily[j],
                                                                   fontface = chooseFace(cur$pars$fontface[j],
                                                                                         cur$pars$font[j]), cex = cur$pars$cex[j]),
                                                         name = componentName("text", componentx,
                                                                              componenty)), row = yy, col = xx)
                } else if (cur$type == "rectangles") {
                    key.gf <- placeGrob(key.gf, rectGrob(height = cur$pars$height[j],
                                                         width = cur$pars$size[j]/max(cur$pars$size),
                                                         default.units = "npc", gp = gpar(alpha = cur$pars$alpha[j],
                                                                                          fill = cur$pars$col[j], col = cur$pars$border[j]),
                                                         name = componentName("rect", componentx,
                                                                              componenty)), row = yy, col = xx)
                } else if (cur$type == "lines") {
                    if (cur$pars$type[j] == "l") {
                        key.gf <- placeGrob(key.gf, linesGrob(x = c(0,
                                                                    1) * cur$pars$size[j]/max(cur$pars$size),
                                                              y = c(0.5, 0.5), gp = gpar(col = cur$pars$col[j],
                                                                                         alpha = cur$pars$alpha[j], lty = cur$pars$lty[j],
                                                                                         lwd = cur$pars$lwd[j]), name = componentName("lines",
                                                                                                                                      componentx, componenty)), row = yy, col = xx)
                    } else if (cur$pars$type[j] == "p") {
                        key.gf <- placeGrob(key.gf, pointsGrob(x = 0.5,
                                                               y = 0.5, gp = gpar(col = cur$pars$col[j],
                                                                                  alpha = cur$pars$alpha[j], cex = cur$pars$cex[j],
                                                                                  fill = cur$pars$fill[j], fontfamily = cur$pars$fontfamily[j],
                                                                                  fontface = chooseFace(cur$pars$fontface[j],
                                                                                                        cur$pars$font[j]), fontsize = fontsize.points),
                                                               pch = cur$pars$pch[j], name = componentName("points",
                                                                                                           componentx, componenty)), row = yy, col = xx)
                    } else {
                        key.gf <- placeGrob(key.gf,
                                            linesGrob(
                                                x = c(0, 1) * cur$pars$size[j]/max(cur$pars$size),
                                                y = c(0.5, 0.5),
                                                gp = gpar(col = cur$pars$col[j],
                                                          alpha = cur$pars$alpha[j],
                                                          lty = cur$pars$lty[j],
                                                          lwd = cur$pars$lwd[j]),
                                                name = componentName("lines", componentx, componenty)),
                                            row = yy, col = xx)
                        if (key$divide > 1) {
                            key.gf <- placeGrob(key.gf, pointsGrob(x = (1:key$divide -
                                                                            1)/(key$divide - 1), y = rep(0.5, key$divide),
                                                                   gp = gpar(col = cur$pars$col[j],
                                                                             alpha = cur$pars$alpha[j],
                                                                             cex = cur$pars$cex[j],
                                                                             fill = cur$pars$fill[j],
                                                                             fontfamily = cur$pars$fontfamily[j],
                                                                             fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                                             fontsize = fontsize.points),
                                                                   pch = cur$pars$pch[j],
                                                                   name = componentName("points", componentx, componenty)),
                                                row = yy, col = xx)
                        } else if (key$divide == 1) {
                            key.gf <- placeGrob(key.gf, pointsGrob(x = 0.5,
                                                                   y = 0.5,
                                                                   gp = gpar(col = cur$pars$col[j],
                                                                             alpha = cur$pars$alpha[j],
                                                                             cex = cur$pars$cex[j],
                                                                             fill = cur$pars$fill[j],
                                                                             fontfamily = cur$pars$fontfamily[j],
                                                                             fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                                             fontsize = fontsize.points),
                                                                   pch = cur$pars$pch[j],
                                                                   name = componentName("points", componentx, componenty)),
                                                row = yy, col = xx)
                        }
                    }
                } else if (cur$type == "points") {
                    key.gf <- placeGrob(key.gf, pointsGrob(x = 0.5,
                                                           y  = 0.5,
                                                           gp = gpar(col = cur$pars$col[j],
                                                                     alpha = cur$pars$alpha[j], cex = cur$pars$cex[j],
                                                                     lwd = cur$pars$lwd[j], fill = cur$pars$fill[j],
                                                                     fontfamily = cur$pars$fontfamily[j],
                                                                     fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                                     fontsize = fontsize.points),
                                                           pch = cur$pars$pch[j],
                                                           name = componentName("points", componentx, componenty)),
                                        row = yy, col = xx)
                }
            }
        }
    } else stop("Sorry, align=FALSE is not supported")
    if (draw)
        grid.draw(key.gf)
    key.gf
}

environment(draw.key) <- environment(lattice::xyplot)
assignInNamespace("draw.key", draw.key, ns="lattice")
