#' read json files exported from GEE
#' 
#' @param indir Input directory which contains the target json files
#' @param pattern an optional regular expression. Only file names which match 
#' the regular expression will be returned.
#' @param baseprob the properties name of input point.
#' @param IsSave save or not?
#' 
#' @rdname read_gee
#' @importFrom purrr map reduce
#' @importFrom data.table data.table as.data.table setkeyv fwrite
#' @export
read_jsons.gee <- function(indir,
                           pattern = "phenoflux212.*.geojson",
                           baseprob = c("site", "date"),
                           IsSave = T) {
    files <- dir(indir, pattern, full.names = T) %>%
        set_names(str_extract(., "(?<=_)\\d+[km]{1,2}(?=_buffer)"))
    lst <- llply(files, read_json.gee, baseprob = baseprob, .progress = "text")

    # fix colnames not same in the lst
    names_common <- reduce(llply(lst, names), intersect)
    df <- map(lst, ~.x[, ..names_common]) %>% melt_list("scale") # only for data.table

    setkeyv(df, baseprob)
    setkeyv(df, "scale")

    outfile <- sprintf("%s/%s.csv", indir,
                       str_extract(basename(files[1]), ".*(?=_\\d+[km]{1,2})"))
    if (IsSave) fwrite(df, outfile)
    df
}

#' read json file exported from GEE
#' 
#' @inheritParams read_jsons.gee
#' @param file json file path
#' @param is_buffer buffered or not?
#' 
#' @rdname read_gee
#' @export
read_json.gee <- function(file, baseprob = c("site", "date"), is_buffer){
    if (missing(is_buffer)) is_buffer <- !grepl("_0m_buffer", file)

    lst <- read_json(file)$features %>% map("properties")

    varnames  <- names(lst[[1]])
    baseprob  <- intersect(varnames, baseprob)
    bandNames <- setdiff(varnames, baseprob)

    # get value from every site and every date
    getEach <- function(x){
        FUN  <- mean
        data <- x[bandNames] %>% sapply(function(xi){ FUN(unlist(xi)) })
        prob <- x[baseprob]
        c(prob, data) # return
    }

    if (is_buffer) {
        data <- llply(lst, getEach) #, .progress = "text"
        res  <- transpose(data) %>% map(unlist) %>% as.data.table()
    } else{
        res <- transpose(lst) %>% map(function(list){
            I_null <- sapply(list, is_empty)
            list[I_null] <- NA
            unlist(list)
        }) %>% as.data.table()
    }
    I <- order()
    res #return
}
