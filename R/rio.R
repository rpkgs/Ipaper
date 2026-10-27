#' export data
#' @details Support rda, rds, fst, csv, qs2, qdata, arrow, feather
#' @importFrom qs2 qd_save qs_save qd_read qs_read
#' @export
export <- function(x, path, ..., nthreads = 6) {
  ext <- tools::file_ext(path) %>% tolower()
  if (ext == "rda") {
    var <- deparse(substitute(x))
    eval(parse(text = glue("save({var}, ..., file = path, envir = parent.frame())")))
    # print(var)
  } else if (ext == "rds") {
    saveRDS(x, path, ...)
  } else if (ext == "fst") {
    write_fst(x, path, ...)
  } else if (ext == "csv") {
    fwrite(x, path, ...)
  } else if (ext %in% c("qs", "qs2")) {
    qs_save(x, path, ..., nthreads = nthreads) # qs also used qs2
  } else if (ext %in% c("qdata")) {
    qd_save(x, path, ..., nthreads = nthreads)
  } else if (ext %in% c("arrow", "feather")) {
    arrow::write_feather(x, path, ...)
  } else {
    message("unsupported file type!")
  }
}


#' import data to R
#' @details Support rda, rds, fst, csv, qs2, qdata, arrow, feather
#' @inheritParams qs2::qd_read
#' @export
import <- function(path, ..., nthreads = 6) {
  ext <- tools::file_ext(path) %>% tolower()
  if (ext == "rda") {
    load(path, envir = parent.frame())
  } else if (ext == "rds") {
    readRDS(path)
  } else if (ext == "fst") {
    import_fst(path, ...)
  } else if (ext == "csv") {
    fread(path, ...)
  } else if (ext %in% c("qs", "qs2")) {
    qs2::qs_read(path, ..., nthreads = nthreads)
  } else if (ext == "qdata") {
    qs2::qd_read(path, ..., nthreads = nthreads)
  } else if (ext %in% c("arrow", "feather")) {
    arrow::read_feather(path, ...)
  } else {
    message("unsupported file type!")
  }
}
