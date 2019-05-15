#' merge_pdf
#'
#' rely on python pdfmerge package, `pip install pdfmerge`
#' 
#' @param outfile String
#' @param indir Directory to search pdfs
#' @param pattern string used to match pdf filename
#' @param del Booolean. If true, after merge, original pdf files will be delete.
#' 
#' @examples
#' \dontrun{
#' merge_pdf("RPlot.pdf", indir = 'Figure', pattern = "*.pdf") 
#' }
#' @export
merge_pdf <- function(outfile = "RPlot.pdf", indir = 'Figure', pattern = "*.pdf", del = FALSE){
    files <- dir(indir, pattern, full.names = TRUE)
    cmd <- sprintf("pdfmerge -o %s %s", outfile, paste(files, collapse = " "))

    system(cmd, wait = del)
    # system(sprintf('pdfmerge -o %s %s', outfile, pattern) )
    if (del) file.remove(files)
}
