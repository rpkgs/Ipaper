# library(openxlsx)
# library(floodmap)
# library(reshape2)
# library(plyr)

# library(httr)
# library(xml2)
# library(magrittr)
# library(stringr)

# source("E:/GitHub/RCurl_project/R/MainFunction.R", encoding = "utf-8")
# httpheader: used to cheat web server
header <- "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"

## global functions -------------------------------------------------------
#' write_urls
#' 
#' write character vectors of urls into text file, in order to the use of 
#' subsequent aria2 download
#' 
#' @param save urls into txt
#' @export
write_urls <- function(urls, file){
  # data.table::fwrite(data.frame(urls), file, col.names = F)
  write.table(data.frame(urls), file, col.names = F, row.names = F, quote=F)
}

#' get DOIs from endnote export xml files
#' 
#' @export 
get_DOI <- function(x) {
  xml_find_first(x, "electronic-resource-num") %>% xml_text() %>% gsub("\r\n| ", "", .)
}

#' check outdir and doi
#' 
#' Check whether outdir exist, if not then will create it. If doi was URLencoded, 
#' then decode it.
#' 
#' @param doi charater or caracter vector are also support. This function also
#' can also decode URLdecode format doi. So doi, like this 
#' "10.1175\%2FJHM-D-15-0157.1" is also support.
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
#' download papers batchly using aria2. `srcFUN` function is used 
#' to generate pdf download links. You can also construct src function personally 
#' to support more.
#' 
#' @param DOIs according to doi, it find corresponding paper and download it. 
#' such as "10.1175/JHM-D-15-0157.1". URLencoding format, like 
#' "10.1175\%2FJHM-D-15-0157.1" is also support.
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
#' DOIs <- rep("10.1175%2FJHM-D-15-0157.1", 4) #test aria2 parallel download
#' download_aria2(DOIs, journal = "JHM", srcFUN = src_AMS, n = 4, Rshell = TRUE)
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
  writeLines(cmd, "clipboard") 
  if (Rshell) shell(cmd)
}

#' download using httr
#' 
#' Download using httr package by hadley
#' @param DOIs according to doi, it find corresponding paper and download it. 
#' such as "10.1175/JHM-D-15-0157.1". URLencoding format, like 
#' "10.1175\%2FJHM-D-15-0157.1" is also support.
#' @param journal journal name, string used to make new directory to save papers
#' 
#' @author Dongdong Kong \url{kongdd@live.cn}
#' 
#' @examples
#' # First, you need to get doi;
#' # Second, you need to select the suitable srcFUN of corresponding database. If
#' #   this package have not yet, you can consider to extend the srcFUN, or 
#' #   contact me directly.
#' 
#' download_httr("10.1175%2FJHM-D-15-0157.1", journal = '.', srcFUN = src_AMS)
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