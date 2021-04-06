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
