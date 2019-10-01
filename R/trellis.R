# trellis.par.get()$layout.widths %>% .[order(names(.))] %>% str()
# trellis.par.get()$layout.heights %>% .[order(names(.))] %>% str()

opt_trellis_strip <- list(
    layout.heights = list(
        top.padding       = 0,
        main.key.padding  = 0,
        key.axis.padding  = 0,
        axis.xlab.padding = 0,
        axis.top = 1, 
        xlab.key.padding  = 0,
        key.sub.padding   = 0,
        bottom.padding    = 0, 
        strip = 1.2
    ), 
    layout.widths = list(
        left.padding      = 0, # left
        right.padding     = 0, # right
        key.ylab.padding  = 0, # axis.y padding
        key.left          = 1.5, 
        key.right         = 1, 
        ylab.axis.padding = 0, # same as above
        axis.key.padding  = 0, # legend left padding
        axis.left = 1, 
        axis.right = 1
    ), 
    # axis.line = list(col = "white"),
    axis.components = list(
        left = list(pad1 = 0), 
        right = list(pad1 = 0), 
        top = list(pad1 = 0.5), 
        bottom = list(pad1 = 0.5) 
    )
)
# List of 15
#  $ axis.key.padding : num 1
#  $ axis.left        : num 1
#  $ axis.panel       : num 1
#  $ axis.right       : num 1
#  $ between          : num 1
#  $ key.left         : num 1
#  $ key.right        : num 1
#  $ key.ylab.padding : num 0
#  $ left.padding     : num 1
#  $ panel            : num 1
#  $ right.padding    : num 1
#  $ strip.left       : num 1
#  $ ylab             : num 1
#  $ ylab.axis.padding: num 1
#  $ ylab.right       : num 1
## 
opt_trellis <- list(
    layout.heights = list(
        top.padding       = 0,
        main.key.padding  = 0,
        key.axis.padding  = 0,
        axis.xlab.padding = 0,
        axis.top = 0, 
        xlab.key.padding  = 0,
        key.sub.padding   = 0,
        bottom.padding    = 1.5, 
        strip = 0
    ), 
    layout.widths = list(
        left.padding      = 0.5, # left
        right.padding     = 0  , # right
        key.ylab.padding  = 0  , # axis.y padding
        ylab.axis.padding = 0  , # same as above
        axis.key.padding  = 0.5, # legend left padding
        axis.left = 1, 
        axis.right = 1
    ), 
    axis.line = list(col = "white"),
    axis.components = list(
        left   = list(pad1 = 0), 
        right  = list(pad1 = 0), 
        top    = list(pad1 = 0), 
        bottom = list(pad1 = 1.5) 
    )
)


opt_trellis_default <- list(
    layout.heights = list(
        top.padding       = 0,
        main.key.padding  = 0,
        key.axis.padding  = 0,
        axis.xlab.padding = 0,
        axis.top = 1, 
        xlab.key.padding  = 1,
        key.sub.padding   = 1,
        bottom.padding    = 2, 
        strip = 1
    ), 
    layout.widths = list(
        left.padding      = 0, # left
        right.padding     = 0, # right
        key.ylab.padding  = 0, # axis.y padding
        key.left          = 1, 
        key.right         = 1, 
        ylab.axis.padding = 0, # same as above
        axis.key.padding  = 0, # legend left padding
        axis.left = 1, 
        axis.right = 1
    ), 
    # axis.line = list(lwd = 1), 
    # axis.line = list(col = "white"),
    axis.components = list(
        left = list(pad1 = 1), 
        right = list(pad1 = 0), 
        top = list(pad1 = 0.5), 
        bottom = list(pad1 = 1) 
    )
)

# opt_trellis_default$axis.line$lwd = 1
