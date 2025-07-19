#' @export
prepend_path <- function(NAME, VALUE, head = FALSE) {
  vals <- Sys.getenv(NAME)
  vals <- if (vals == "") {
    VALUE
  } else {
    if (head) paste0(vals, ":", VALUE) else paste0(VALUE, ":", vals)
  }
  cmd <- sprintf("Sys.setenv(%s=vals)", NAME)
  eval(parse(text = cmd))
}
