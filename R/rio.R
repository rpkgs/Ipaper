#' Write a data file, inferring the format from the extension
#' @details Supports rda, rds, fst, csv, qs, qs2, qdata, arrow, and feather.
#' @param x Object to write.
#' @inheritParams auto_read
#' @importFrom qs2 qd_save qs_save qd_read qs_read
#' @export
auto_write <- function(x, path, ..., nthreads = 6) {
  path <- glue(path, .envir = parent.frame())
  ext <- tools::file_ext(path) %>% tolower()
  if (ext == "rda") {
    var <- deparse(substitute(x))
    save(list = var, ..., file = path, envir = parent.frame())
    # print(var)
  } else if (ext == "rds") {
    saveRDS(x, path, ...)
  } else if (ext == "fst") {
    write_fst(x, path, ...)
  } else if (ext == "csv") {
    fwrite(x, path, ..., bom = TRUE) # !add BOM for UTF-8 encoding
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

#' Read a data file, inferring the format from the extension
#' @details Supports rda, rds, fst, csv, qs, qs2, qdata, arrow, and feather.
#' @param path Path to the data file. Supports glue templates such as
#'   `"{stem}.rds"`. Expressions inside `{}` are evaluated in the caller's
#'   environment.
#' @param ... Arguments passed to the format-specific reader or writer.
#' @param nthreads Number of threads to use for qs2 and qdata files.
#' @export
auto_read <- function(path, ..., nthreads = 6) {
  path <- glue(path, .envir = parent.frame())
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

#' @rdname auto_read
#' @export
read_auto <- auto_read

#' @rdname auto_write
#' @export
write_auto <- auto_write

#' Deprecated dataset I/O functions
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Use [auto_read()] instead of `import()` and [auto_write()] instead of
#' `export()`.
#' @inheritParams auto_write
#' @name Ipaper-deprecated
NULL

#' @rdname Ipaper-deprecated
#' @export
import <- function(path, ..., nthreads = 6) {
  lifecycle::deprecate_warn("0.1.11", "import()", "auto_read()")

  # Keep the caller's environment and argument expressions for .rda files.
  call <- sys.call()
  call[[1L]] <- auto_read
  eval(call, parent.frame())
}

#' @rdname Ipaper-deprecated
#' @export
export <- function(x, path, ..., nthreads = 6) {
  lifecycle::deprecate_warn("0.1.11", "export()", "auto_write()")

  call <- sys.call()
  call[[1L]] <- auto_write
  eval(call, parent.frame())
}
