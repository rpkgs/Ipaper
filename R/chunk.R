#' chunk
#' 
#' @references https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
#' @export
chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
