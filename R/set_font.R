#' @import showtext
#' @export
showtext::showtext_auto

#' settting font family for plot
#'
#' @export
#' @keywords internal
set_font <- function() {
  # OS.type = .Platform$OS.type
  # if (OS.type == 'windows') {
  #   grDevices::windowsFonts(
  #     Times       = grDevices::windowsFont("Times New Roman"),
  #     Arial       = grDevices::windowsFont("Arial"),
  #     hwfsong     = grDevices::windowsFont("STSong"),
  #     song        = grDevices::windowsFont("SimSun"),
  #     fsong       = grDevices::windowsFont("FangSong"),
  #     rTimes      = grDevices::windowsFont("rTimes"),
  #     TimesSimSun = grDevices::windowsFont("TimesSimSun"),
  #     YH          = grDevices::windowsFont("Microsoft Yahei"),
  #     whit        = grDevices::windowsFont("Whitney-Book")
  #   )
  # } else if (OS.type == 'unix'){
  #   Cairo::CairoFonts(
  #     regular    = "Times New Roman:style=Regular",
  #     bold       = "Times New Roman:style=Bold",
  #     italic     = "Times New Roman:style=Oblique",
  #     bolditalic = "Times New Roman:style=BoldOblique"
  #   )
  # }
  fonts <- c(
    Times = "Times", Arial = "Arial",
    hwfsong = "STSong", fsong = "simfang", song = "SimSun", YH = "msyh",
    rTimes = "rTimes", TimesSimSun = "TimesSimSun"
  )
  add_fonts(fonts)

  showtext_auto(TRUE)
}

#' @export
#' @rdname set_font
clear_font <- function() {
  showtext_auto(FALSE)
}


guess_font_path <- function(fonts) {
  info <- sysfonts::font_files()
  info <- info[order(info$face != "Regular"), ]
  fonts_find <- tools::file_path_sans_ext(info$file)
  ind <- match(tolower(fonts), tolower(fonts_find))

  # Match family names when font filenames differ across systems.
  missing <- is.na(ind)
  ind[missing] <- match(tolower(fonts[missing]), tolower(info$family))
  keep <- !is.na(ind)
  info <- info[ind[keep], ]
  set_names(file.path(info$path, info$file), names(fonts)[keep])
}

#' @importFrom sysfonts font_add
add_fonts <- function(fonts) {
  fonts_path <- guess_font_path(fonts)
  for (i in seq_along(fonts_path)) {
    family <- names(fonts_path)[i]
    path <- fonts_path[i]
    tryCatch({
      font_add(family, path)
    }, error = function(e) {
      message(sprintf("[%s]: %s", family, e$message))
    })
  }
}
