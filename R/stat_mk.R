# StatChull <- ggproto("StatChull", Stat,
#   compute_group = function(data, scales) {
#       browser()
#     data[chull(data$x, data$y), , drop = FALSE]
#   },  
#   required_aes = c("x", "y")
# )
# stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
#                        position = "identity", na.rm = FALSE, show.legend = NA,
#                        inherit.aes = TRUE, ...) {
#     layer(
#         stat = StatChull, data = data, mapping = mapping, geom = geom,
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#         params = list(na.rm = na.rm, ...)
#     )
# }

StatMK <- ggproto("StatLm", Stat,
    required_aes = c("x", "y"),
    compute_group = function(data, scales, ...) {
        rng <- range(data$x, na.rm = TRUE)
        grid <- data.frame(x = rng)
        mod = mkTrend_rcpp(data$y, data$x)
        grid$y = grid$x * mod["slp"] + mod["intercept"]
        # browser()
        # mod <- lm(y ~ x, data = data)
        # grid$y <- predict(mod, newdata = grid)
        grid
    }
)

#' stat_mk
#' @example man/examples/ex-stat_mk.R
#' @export 
stat_mk <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
    layer(
        stat = StatMK, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
