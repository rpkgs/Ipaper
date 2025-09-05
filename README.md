
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ipaper

<!-- badges: start -->

[![R-CMD-check](https://github.com/rpkgs/Ipaper/workflows/R-CMD-check/badge.svg)](https://github.com/rpkgs/Ipaper/actions)
[![codecov](https://codecov.io/gh/rpkgs/Ipaper/branch/master/graph/badge.svg)](https://codecov.io/gh/rpkgs/Ipaper)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![CRAN](http://www.r-pkg.org/badges/version/rwiki)](https://cran.r-project.org/package=rwiki)
<!-- badges: end -->

## Installation

``` r
# devtools::install_github("kongdd/Ipaper")
devtools::install_git("https://gitee.com/adv-r/Ipaper")
# or from gz file
install.packages("../Ipaper_0.1.5.9000.tar.gz", repos = NULL, type = "source", dependencies = TRUE)
install.packages("Ipaper", repos = "https://rpkgs.r-universe.dev")
# $sudo apt-get install libfftw3-dev
```

## FUNCTIONS

### `rstudio` shortcuts addin

| Description                                       | Shortcut     |
|---------------------------------------------------|--------------|
| copy lines at the cursor position (sublime style) | Alt+C        |
| clip lines at the cursor position                 | Ctrl+X       |
| Select word                                       | Ctrl+D       |
| Insert `%<>%`                                     | Ctrl+Shift+, |
| Insert `%do% {}`                                  | Ctrl+Shift+D |
| Insert `%in% {}`                                  | Ctrl+Shift+I |
| Open in VSCode                                    | Alt+Shift+V  |
| Open in smerge                                    | Ctrl+Shift+G |
| Open in subl                                      | Alt+Shift+L  |

Note that `VSCode`, `smerge` or `subl` should be in the system path, if you want to use it.

After install, run `Ipaper::key_blind()` to make those shortcuts work.

### Visualization

<!-- -   `draw.colorkey`: modified from lattice, add triangle head and tail

    ![](man/figures/lgd_draw.colorkey.svg)

-   `geom_boxplot2`: boxplot without outlier

    ![](man/figures/geom_boxplot2.svg) -->

<!-- -   `ggplot_legend`: get the legend (grid obj) of ggplot object -->

-   `write_fig`: Unify figure writing functions, e.g. png, pdf, tif, svg

### Parallel functions

-   `InitCluster`, `killCluster`.
-   `llply_par`, `apply_par`

### Base tools

-   `code`, `smerge`, `subl`, `github`: open app at assigned directory
-   `mkdir`, `touch`
-   `dir.show`: open at assigned directory in explorer
-   `runningId`: print the running ID in the console
-   `fprintf`: c style `fprintf`
-   `melt_list`, `melt_tree`, `listk`, `dcast2`

``` r
df <- data.frame(year = 2010, day = 1:2, month = 1, site = "A")
l  <- list(a = df, b = df)
melt_list(l, "id")
#>    id year day month site
#> 1:  a 2010   1     1    A
#> 2:  a 2010   2     1    A
#> 3:  b 2010   1     1    A
#> 4:  b 2010   2     1    A

l2 <- listk("type1" = l, "type2" = l)
melt_tree(l2, c("type", "id"))
#>     type id year day month site
#> 1: type1  a 2010   1     1    A
#> 2: type1  a 2010   2     1    A
#> 3: type1  b 2010   1     1    A
#> 4: type1  b 2010   2     1    A
#> 5: type2  a 2010   1     1    A
#> 6: type2  a 2010   2     1    A
#> 7: type2  b 2010   1     1    A
#> 8: type2  b 2010   2     1    A
```

-   `reoder_name`, `rm_empty`, `match2`
-   `write_list2xlsx`, `read_xlsx2list`
-   `which.na`, `which.notna`, `which.isnull`, `which.notnull`

## Updates

-   `mkTrend` and `slope` has been moved to the package
    [`rtrend`](https://cran.rstudio.com/web/packages/rtrend/index.html)
