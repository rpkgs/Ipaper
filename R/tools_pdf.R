#' Convert PDF to EMF
#'
#' Convert a PDF file to an EMF file. Requires \code{inkscape} command-line utility.
#'
#' The function creates an EMF file at the same location where the PDF is.
#'
#' @param filename A PDF file name or file path
#'
#' @keywords internal
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' pdf_to_emf(filename = "~/test.pdf")
#' }
pdf_to_emf <- function(filename) {
    input <- normalizePath(filename)
    output <- gsub("\\.pdf$", "\\.emf", input)
    expr <- paste0("inkscape --file=", input, " --export-emf=", output)
    system(expr)
}

#' merge_pdf
#'
#' rely on python pdfmerge package, `pip install pdfmerge`
#'
#' @param outfile String
#' @param indir Directory to search pdfs
#' @param pattern string used to match pdf filename
#' @param del Booolean. If true, after merge, original pdf files will be delete.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' merge_pdf("RPlot.pdf", indir = "Figure", pattern = "*.pdf")
#' }
#' @export
merge_pdf <- function(outfile = "RPlot.pdf", indir = "Figure",
                      pattern = "*.pdf", del = FALSE) {
    files <- dir(indir, pattern, full.names = TRUE)
    order <- str_extract(basename(files), "(?<=\\[)\\d*(?=.*\\])") %>%
        as.numeric()
    if (all(is.finite(order))) files <- files[order]
    pdftools::pdf_combine(files, outfile)
    if (del) file.remove(files)
}

# merge_pdf <- function(outfile = "RPlot.pdf", indir = "Figure", pattern = "*.pdf", del = FALSE) {
#     files <- dir(indir, pattern, full.names = TRUE)
#     order <- str_extract(basename(files), "(?<=\\[)\\d*(?=.*\\])") %>% as.numeric()
#     if (all(is.finite(order))) files <- files[order]
#     cmd <- sprintf("pdfmerge -o %s %s", outfile, paste(files, collapse = " "))
#     shell(cmd, wait = del)
#     # system(sprintf('pdfmerge -o %s %s', outfile, pattern) )
#     if (del) file.remove(files)
# }
