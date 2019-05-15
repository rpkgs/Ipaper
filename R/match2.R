#' @export
match2 <- function(x, y) {
    I <- match(x, y) 
    I_x <- which.notna(I)
    I_y <- I[I_x]

    d <- data.table(x = x[I_x], y = y[I_y], I_x, I_y, 
        grp = cumsum(c(TRUE, diff(I_y) != 1)))
    d
}
