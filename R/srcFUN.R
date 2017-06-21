#' srcFUN
#' 
#' simplest srcFUN, just treat doi as download links  
#' @export
src_URL <- function(doi) doi

#' srcFUN of WILEY library
#' 
#' Journal like GRL, JGR, WRR, HP, all in the database. 
#' Compared with other srcFUNs, this one is quite complicated. It has to base on 
#' previous web page request of identidy authentication. So it can't download simply
#' base on pdf urls, likes other database.
#' This function was need to further test, whether this function can support 
#' download with urls
#' @param  srcDownload If true, it will will download pdf directly, and return 
#' pdf src. If false, only pdf src returned, without downlaoding pdf.
#' @param ... other parameters pass to httr::GET
#' @export
src_wiley_I <- function(doi, outdir = "./", srcDownload = TRUE, ...){
  doi %<>% Init_Check(outdir)
  
  url <- paste0("http://onlinelibrary.wiley.com/doi/", doi, "/pdf")
  p <- GET(url, add_headers(`User-Agent`= header))
  
  src <- content(p) %>% xml_find_all("//iframe[@id='pdfDocument']") %>% xml_attr("src")
  # file_pdf <- str_extract(src, ".*pdf") %>% basename %>% paste0(outdir, .)
  if (srcDownload) write_webfile(src, outdir, ...)
  return(src)
}

#' srcFUN of American Meteorological Society. 
#' @export
src_AMS <- function(doi){
  paste0("http://journals.ametsoc.org/doi/pdf/", doi)
}
#' srcFUN of Springer
#' @export
src_Springer <- function(doi){
  paste0("https://link.springer.com/content/pdf/", DOIs, ".pdf")
}

#' srcFUN of Scientific Reports
#' @export
src_SciReps <- function(doi){
  gsub("10.1038/", "", doi) %>% 
    paste0("https://www.nature.com/articles/", ., ".pdf")
} 

#' srcFUN of IOPscience database
#' @export
src_IOP <- function(doi){
  paste0("http://iopscience.iop.org/article/", doi, "/pdf")
}

#' srcFUN of hess
#' 
#' Generate paper of HESS journal pdf download source link
#' @param doi DOI like this format "10.5194/hess-20-4191-2016"
#' @export
src_hess <- function(doi){
  # doi %<>% {strsplit(., "/")[[1]][2]}
  doi <- strsplit(doi, "/") %>% laply(function(x) x[2])
  # replaceString <- strsplit(doi[1], "/")[[1]][1]
  #  	doi %<>% gsub(paste0(replaceString, "/"), "", .)
  paste0("http://www.hydrol-earth-syst-sci.net/", 
         gsub("hess-", "", doi) %>% gsub("-", "/", .), "/", 
         doi, ".pdf")
}

#' srcFUN of elsevier database
#' @description Just pdf src returned. If you want to download directly you 
#' can use download_httr(doi, journal = '.', srcFUN = src_SciDirect)
#' @export
src_SciDirect <- function(doi){
  src <- tryCatch({
    p <- POST("http://dx.doi.org/", encode = "form",
              body = list(hdl = doi)) %>% content(encoding = "UTF-8")
    json <- xml_find_first(p, "//script[@type='application/json']") %>% xml_text
    
    if (is.na(json)){
      href <- xml_find_first(p, "//a[@id='pdfLink']") %>% xml_attr("href")
      # or "//div[@class='PdfDropDownMenu']"
    }else{
      href <- fromJSON(json)$article$pdfDownload$linkToPdf %>% 
        paste0("http://www.sciencedirect.com", .)
    }
    href#return trycatch
  }, error = function(e) {
    message(e)
    return("")
  })
  return(src)
}