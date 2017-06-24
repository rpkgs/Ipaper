#' download web file through web link src
#' 
#' #http://www.sciencedirect.com/science/article/pii/S0034425701002310/pdfft?md5=2e6dfdaa5b680d49fbe09360b5bed6b4&pid=1-s2.0-S0034425701002310-main.pdf
#' # has an error: for above url
#' 
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
#' @param DOIs according to doi, it find corresponding paper and download it. 
#' such as "10.1175/JHM-D-15-0157.1". URLencoding format, like 
#' "10.1175\%2FJHM-D-15-0157.1" is also support.
#' @param outdir output file directory
#' @param srcDownload If true, it will will download pdf directly, and return 
#' pdf src. If false, only pdf src returned, without downlaoding pdf.
#' @param ... other parameters pass to httr::GET
#' @export
src_wiley_I <- function(DOIs, outdir = "./", srcDownload = TRUE, ...){
  DOIs %<>% Init_Check(outdir)
  
  urls <- paste0("http://onlinelibrary.wiley.com/doi/", DOIs, "/pdf")
  # for every single url; modified to support batch download model
  FUN <- function(url){
    tryCatch({
      p <- GET(url, add_headers(`User-Agent` = header)) %>% content(p)
      src <- xml_find_all(p, "//iframe[@id='pdfDocument']") %>% xml_attr("src")
      # file_pdf <- str_extract(src, ".*pdf") %>% basename %>% paste0(outdir, .)
      if (srcDownload) write_webfile(src, outdir, ...)
      return(src)
    }, error = function(e) {
      message(e)
      return("")
    })
  }
  
  sapply(DOIs, FUN, USE.NAMES = FALSE)#return srcs
}

#' srcFUN of elsevier database
#' @description Just pdf src returned. If you want to download directly you 
#' can use download_httr(doi, journal = '.', srcFUN = src_SciDirect)
#' 
#' @param DOIs according to doi, it find corresponding paper and download it. 
#' such as "10.1175/JHM-D-15-0157.1". URLencoding format, like 
#' "10.1175\%2FJHM-D-15-0157.1" is also support.
#' @param outdir output file directory
#' @export
src_SciDirect_I <- function(DOIs, outdir = "./", srcDownload = TRUE, ...){
  DOIs %<>% Init_Check(outdir)
  FUN <- function(doi){
    tryCatch({
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
      # file <- paste0(outdir, doi, ".pdf")
      if (srcDownload) write_webfile(src, outdir, ...)
      href#return trycatch
    }, error = function(e) {
      message(e)
      return("")
    })
  }
  sapply(DOIs, FUN, USE.NAMES = FALSE)#return srcs
  # return(src)
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