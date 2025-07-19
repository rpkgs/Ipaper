clean_str_deg <- function(x) {
  stringr::str_replace_all(x, c(
    "˚" = "°",
    "′|ʹ" = "'",
    "″|''" = "\"",
    "E|N" = "",
    " " = ""
  )) %>% stringr::str_trim()
}

.parse_deg <- function(x) {
  fmt_num <- "(\\d+\\.?\\d*)"
  # decimal degree
  if (str_detect(x, "^\\d+(\\.\\d+)?°?$")) {
    return(as.numeric(str_replace_all(x, "[°$]", "")))
  }

  # degree, minute, second
  if (str_detect(x, glue("^\\d+°\\d+'{fmt_num}"))) {
    parts <- as.numeric(unlist(str_extract_all(x, "\\d+")))
    return(parts[1] + parts[2] / 60 + parts[3] / 3600)
  }

  # degree, minute
  if (str_detect(x, glue("^\\d+°{fmt_num}'$"))) {
    parts <- as.numeric(unlist(str_extract_all(x, "\\d+")))
    return(parts[1] + parts[2] / 60)
  }
  return(as.numeric(x))
}

#' parse_deg
#'
#' @description
#' - `*`: zero or more
#' - `+`: one or more
#' - `?`: zero or one
#' @importFrom stringr str_detect str_replace_all str_extract_all str_trim
#' @export
parse_deg <- function(xs) {
  sapply(xs %>% clean_str_deg(), .parse_deg) %>% setNames(NULL)
}
