#' @importFrom xml2 read_xml xml_find_first xml_find_all xml_text
#' @export
url_filezilla <- function(file) {
    p <- read_xml(file)
    
    host  <- xml_find_first(p, "//Host") %>% xml_text() %>% paste0("ftp://", .)
    paths <- xml_find_all(p, "//File/RemotePath") %>% xml_text() %>% 
        gsub("1 0", "", .) %>% gsub(" \\d{1,} ", "/", .)
    files <- xml_find_all(p, "//File/RemoteFile") %>% xml_text() %>% paste0(host, paths, "/", .)
    data.table::fwrite(data.frame(files), "urls.txt", col.names = FALSE)
}
