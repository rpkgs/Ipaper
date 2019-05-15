#' print the running ID in the console
#' 
#' @param i the running Id.
#' @param N The number of total iteration 
#' @param step how long of print step.
#' @param prefix prefix string
#' 
#' @examples
#' for (i in 1:10){
#'     runningId(i, prefix = "phenofit")
#' }
#' @export
runningId <- function(i, step = 1, N, prefix = "") {
    perc <- ifelse(missing(N), "", sprintf(", %.1f%% ", i/N*100))
    if (mod(i, step) == 0) cat(sprintf("[%s] running%s %d ...\n", prefix, perc, i))    
}
