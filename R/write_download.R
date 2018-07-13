# library(openxlsx)
# library(reshape2)
# library(plyr)

# library(httr)
# library(xml2)
# library(magrittr)
# library(stringr)

# source("E:/GitHub/RCurl_project/R/MainFunction.R", encoding = "utf-8")
# httpheader: used to cheat web server
header <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.94 Safari/537.36"

# http://www.sciencedirect.com/science/article/pii/S0034425701002310/pdfft?md5=2e6dfdaa5b680d49fbe09360b5bed6b4&pid=1-s2.0-S0034425701002310-main.pdf
# has an error: for above url

#' write_webfile
#' 
#' download web file through web link src
#' 
#' @param src download link
#' @param outdir output directory
#' @param file file name
#' @param ... other parameters to \code{\link[httr]{GET}}
#' @export
write_webfile <- function(src, outdir = "./", file = NULL, ...){
  # extract pdf filename from src, and combine with outdir
  if (is.null(file)) {
    # strs <- strsplit(src, "\\?")[[1]]
    # file <- str_extract(basename(strs[1]), ".*pdf$") %>% paste0(outdir, .)

    # match '-', 'alphabet and number', '\\.'
    file <- str_extract(src, "[-\\w\\.]*\\.pdf")[[1]][1] %>% paste0(outdir, .)
  }else{
    #make sure outdir is correct, the last character of outdir, should be '/'
    file <- paste0(outdir, basename(file)) 
  }
  
  # IF file exist then break out the function
  if (!file.exists(file)){
    tryCatch({
      GET(src, add_headers(`User-Agent` = header),
          write_disk(file, overwrite = TRUE), progress(), ...)
      cat("\n") #offset the deficiency of progress (without newline at the end)
    }, 
    error = function(e) {
      message(e)
      return(e)
    })
  }
}

#' write_urls
#' 
#' write character vectors of urls into text file, in order to the use of 
#' subsequent aria2 download
#' 
#' @param urls pdf downloading urls.
#' @param file urls are written into file.
#' 
#' @importFrom utils URLdecode write.table
#' @export
write_urls <- function(urls, file){
  # data.table::fwrite(data.frame(urls), file, col.names = F)
  write.table(data.frame(urls), file, col.names = F, row.names = F, quote=F)
}

#' get DOIs from endnote export xml files
#' 
#' @param xmlfile Endnote exported xml file path, DOI information must be included.
#' @export
getDOIs_endnote <- function(xmlfile) {
    doc    <- read_xml(xmlfile)
    titles <- xml_find_all(doc, "//title") %>% xml_text()
    dois   <- xml_find_all(doc, "//electronic-resource-num") %>% xml_text() %>% 
        gsub("\r\n| ", "", .)

    data.frame(title = titles, DOI = dois, stringsAsFactors = F)#quickly return
}

#' check outdir and doi
#' 
#' Check whether outdir exist, if not then will create it. If doi was URLencoded, 
#' then decode it.
#' 
#' @inheritParams src_wiley_I
#' 
#' @return URLdecode doi 
#' @export
Init_Check <- function(doi, outdir){
  if (!dir.exists(outdir)) dir.create(outdir)
  sapply(doi, URLdecode, USE.NAMES = F)
}

# download_AMS_I <- function(doi, outdir, ...){
#   doi %<>% Init_Check(outdir)
#   url <- paste0("http://journals.ametsoc.org/doi/pdf/", doi)
  
#   file_pdf <- paste0(outdir, doi, ".pdf") 
#   write_webfile(src, file_pdf, ...)
# }

# downlaod paper from springer
# download_Springer_I <- function(doi, outdir, ...){
#   doi %<>% Init_Check(outdir)
#   src <- paste0("https://link.springer.com/content/pdf/", doi, ".pdf")

#   file_pdf <- paste0(outdir, doi, ".pdf") 
#   write_webfile(src, file_pdf, ...)
# }

#' download using aria2
#' 
#' download papers batchly using aria2. \code{srcFUN} function is used 
#' to generate pdf download links. You can also construct src function personally 
#' to support more.
#' 
#' @inheritParams src_wiley_I
#' @param journal journal name, string used to make new directory to save papers
#' @param srcFUN function used to generate pdf download links according to DOIs. 
#' If srcFUN is null, then this function will treat input parameter DOIs as download urls.
#' @param n maximum number of parallel downloads. We used aria2c to download 
#' papers batchly. In this function we set --max-concurrent-downloads, --split and  
#' --max-connection-per-server all equal to n. Detailed information about this
#' parameters could be find in aria2c document \url{https://aria2.github.io/manual/en/html/}. 
#' @param Rshell whether execute aria2c command in R shell. If false, it will save
#' command string to clipboard, and then you can paste this command in cmd OR other 
#' shells which can execute aria2c command
#' @param ... other parameter pass to srcFUN.
#' 
#' @author Dongdong Kong \url{kongdd@live.cn}
#' 
#' @examples
#' # First, you need to get doi;
#' # Second, you need to select the suitable srcFUN of corresponding database. If
#' #   this package have not yet, you can consider to extend the srcFUN, or 
#' #   contact me directly.
#' 
#' \dontrun{
#' DOIs <- rep("10.1175%2FJHM-D-15-0157.1", 4) #test aria2 parallel download
#' download_aria2(DOIs, journal = "JHM", srcFUN = src_AMS, n = 4, Rshell = TRUE)
#' }
#' @export
download_aria2 <- function(DOIs, journal = '.', srcFUN = NULL, n = 8, Rshell = FALSE, ...){
  # if (!dir.exists(journal)) dir.create(journal)
  DOIs <- Init_Check(DOIs,outdir = journal)
  # Convert DOIs to pdf download urls using srcFUN. If srcFUN is NULL, then set urls
  #   equal to DOIs
  if (is.null(srcFUN)){
    urls <- DOIs
  }else{
    urls <- srcFUN(DOIs, ...)  
  }
  
  write_urls(urls, paste0(journal, ".txt"))
  
  # --header "%s"
  cmd <- sprintf('aria2c -x%d -s%d -j%d -k1M -c -i %s.txt -d %s', n, n, n, journal, journal)
  if (.Platform$OS.type == 'windows') writeLines(cmd, "clipboard") 
  if (Rshell) shell(cmd)
}

#' download using httr
#' 
#' Download using httr package by hadley
#' 
#' @inheritParams download_aria2
#' 
#' @examples
#' # First, you need to get doi;
#' # Second, you need to select the suitable srcFUN of corresponding database. If
#' #   this package have not yet, you can consider to extend the srcFUN, or 
#' #   contact me directly.
#' 
#' \dontrun{
#' download_httr("10.1175%2FJHM-D-15-0157.1", journal = '.', srcFUN = src_AMS)
#' }
#' @export
download_httr <- function(DOIs, journal = '.', srcFUN = NULL, ...){
  # if (!dir.exists(journal)) dir.create(journal)
  DOIs <- Init_Check(DOIs,outdir = journal)
  # Convert DOIs to pdf download urls using srcFUN. If srcFUN is NULL, then set urls
  #   equal to DOIs
  if (is.null(srcFUN)) srcFUN <- src_URL

  for (i in seq_along(DOIs)){
    cat(sprintf("[%d]: downloading %s\n", i, DOIs[i]))
    src <- srcFUN(DOIs[i], ...)
    write_webfile(src, outdir = paste0(journal, "/"))
  }
}