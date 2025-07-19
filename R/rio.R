#' @importFrom fst write_fst read_fst
export_fst <- function(x, path, compress = 100, uniform_encoding = TRUE) {
  write_fst(x, path, compress, uniform_encoding)
}

import_fst <- function(
    path, columns = NULL, from = 1, to = NULL,
    as.data.table = TRUE, old_format = FALSE) {
  read_fst(path, columns, from, to, as.data.table, old_format)
}


#' export data
#' @details Support rda, rds, fst, csv, qs
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
  } else if (ext == "qs") {
    qsave(x, path, ..., nthreads = nthreads)
  } else {
    message("unsupported file type!")
  }
}

#' import data to R
#' @details Support rda, rds, fst, csv, qs
#' @inheritParams qs::qread
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
  } else if (ext == "qs") {
    qread(path, ..., nthreads = nthreads)
  } else {
    message("unsupported file type!")
  }
}
