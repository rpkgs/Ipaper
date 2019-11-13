# # update theme

# is_missing_arg <- function(x) identical(x, quote(expr = ))

# modify_list <- function(old, new)  {
#     for (i in names(new)) old[[i]] <- new[[i]]
#     old
# }

# find_args <- function(...) 
# {
#     env <- parent.frame()
#     args <- names(formals(sys.function(sys.parent(1))))
#     vals <- mget(args, envir = env)
#     vals <- vals[!vapply(vals, is_missing_arg, logical(1))]
#     modify_list(vals, list(..., ... = NULL))
# }

# # only for margin
# #' theme_lattice
# #' 
# #' @param plot.margin plot padding in layout.heights and layoutheights
# #' @param key.margin legend margin 
# #' @param axis.margin axis margin
# #' 
# #' @param axis.components.inner,axis.components.outer 
# #' margin object in the order to `"top", "right", "bottom", "left"`.
# #' 
# #' @note key.margin left not work (right controls)
# #' 
# #' @export
# theme_lattice <- function(plot.margin, key.margin, axis.margin, 
#     axis.components.inner,
#     axis.components.outer,
#     ..., 
#     layout.widths,
#     layout.heights, 
#     par.settings) 
# {
#     orders = c("top", "right", "bottom", "left")
#     dots   = list(...)

#     setting = NULL
#      # elements <- find_args(..., complete = NULL, validate = NULL)
#     if (!missing(plot.margin)) {
#         margin = plot.margin %>% as.numeric()
            
#         setting$layout.heights %<>% updateList(list(top.padding   = margin[1], bottom.padding = margin[3]))
#         setting$layout.widths  %<>% updateList(list(right.padding = margin[2], left.padding = margin[4]))
#     }

#     if (!missing(key.margin)) {
#         margin = key.margin %>% as.numeric()
#         setting$layout.heights %<>% updateList(list(key.top   = margin[1], key.bottom = margin[3]))
#         setting$layout.widths  %<>% updateList(list(key.right = margin[2], key.left = margin[4]))   
#     }

#     if (!missing(axis.margin)) {
#         margin = axis.margin %>% as.numeric()
#         setting$layout.heights %<>% updateList(list(axis.top   = margin[1], axis.bottom = margin[3]))
#         setting$layout.widths  %<>% updateList(list(axis.right = margin[2], axis.left = margin[4]))   
#     }

#     if (!missing(axis.components.inner)) {
#         margin = axis.components.inner %>% as.numeric()
#         axis.components = NULL
#         for (i in seq_along(orders)) {
#             axis.components[[orders[i]]]$pad1 = margin[i]
#         }
#         setting$axis.components %<>% updateList(axis.components)
#     }
    
#     if (!missing(axis.components.outer)) {
#         margin = axis.components.outer %>% as.numeric()
#         axis.components = NULL
#         for (i in seq_along(orders)) {
#             axis.components[[orders[i]]]$pad2 = margin[i]
#         }
#         setting$axis.components %<>% updateList(axis.components)
#     }

#     if (!missing(layout.widths)) {
#         setting$layout.widths %<>% updateList(layout.widths)
#     }

#     if (!missing(layout.heights)) {
#         setting$layout.heights %<>% updateList(layout.heights)
#     }

#     if (!is.null(dots)) setting %<>% updateList(dots)

#     if (!missing(par.settings)) {
#         setting %<>% updateList(par.settings)
#     }

#     # browser()
#     # if (!is.null(axis.margin)) {
#     #     margin = axis.margin %>% as.numeric()
#     #     setting$layout.heights <- modifyList(., list(axis.top   = margin[1], axis.bottom = margin[3]))
#     #     setting$layout.widths  <- modifyList(., list(axis.right = margin[2], axis.left = margin[4]))   
#     # }
#     # class(setting) <- "list"
#     setting
# }

# print.theme <- function(x, ...) {
#     print(str(x))
#     invisible()
# }

# updateList <- function(x, val) {
#     if (is.null(x)) x <- list()
#     if (is.null(val)) val <- list()
#     modifyList(x, val)
# }

# #' @export
# `+.trellis` <- function(p, setting){
#     if (!missing(setting)) {
#         if ("layer" %in% class(setting)) {
#             latticeExtra:::`+.trellis`(p, setting)
#         } else {
#             # setting = p$par.settings
#             p$par.settings %<>% updateList(setting)        
#             p
#         }
#     } else p
# }

# #' @importFrom utils modifyList
# ## Test about modifyList

# # par.strip.text = list(cex = 2) 
# layout.heights <- function(
#     main             = 1,
#     main.key.padding = 1,
#     xlab.top         = 1,
#     key.axis.padding = 1,
#     strip            = 1,
#     panel            = 1,
#     axis.panel       = 1,
#     between          = 1,
#     axis.xlab.padding= 1,
#     xlab             = 1,
#     xlab.key.padding = 0,
#     key.sub.padding  = 1,
#     sub              = 1, ...)
# {
#     elements <- find_args(..., complete = NULL, validate = NULL)
#     elements
# }

# layout.widths <- function(
#     axis.key.padding = 1, 
#     axis.panel       = 1, 
#     between          = 1, 
#     key.ylab.padding = 0, 
#     panel            = 1, 
#     strip.left       = 1, 
#     ylab             = 1, 
#     ylab.axis.padding= 1, 
#     ylab.right       = 1, ...)
# {
#     elements <- find_args(..., complete = NULL, validate = NULL)
#     elements 
# }