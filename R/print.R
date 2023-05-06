# #' @importFrom pillar dim_desc 

#' print.data.table
#' 
#' @examples 
#' d = data.table(1:100)
#' options(datatable.print.nrow = 100)
#' print(d)
#' @export
print.data.table <- function(d, n = NULL, ..., maxrows = 1e6) {
  if (is.null(n)) n = getOption("datatable.print.nrow") %||% 10

  # cat(sprintf("[data.table]: %s \n", dim_desc(d)))
  if (nrow(d) <= maxrows) {
    cat("[data.table]: \n")
    
    class(d) <- c("tbl", "data.table", "data.frame")
    print(d, n = n, ...)
  } else {
    data.table:::print.data.table(d)
  }
}
