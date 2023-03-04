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
  fonts = c(Times = "Times", Arial = "Arial", 
    hwfsong = "STSong", fsong = "simfang", song = "SimSun", YH = "msyh",
    rTimes = "rTimes", TimesSimSun = "TimesSimSun")
  add_fonts(fonts)

  showtext_auto(TRUE)
}

#' @export 
#' @rdname set_font
clear_font <- function() {
  showtext_auto(FALSE)
}


guess_font_path <- function(fonts) {
  font_local = paste0(Sys.getenv("LOCALAPPDATA"), "/Microsoft")  
  dir_font = c("c:", font_local)
  
  info = expand.grid(dir = dir_font, font = fonts, type = c("ttf", "ttc"))

  fs = sprintf("%s/Windows/Fonts/%s.%s", 
    info$dir, info$font, info$type)
  fs = fs[file.exists(fs)]

  fonts_find = basename(fs) %>% stringr::str_extract(".*(?=\\.)")
  ind = match(fonts_find, fonts)
  set_names(fs, names(fonts[ind]))
}

#' @importFrom sysfonts font_add
add_fonts <- function(fonts) {
  fonts_path = guess_font_path(fonts)
  for(i in seq_along(fonts_path)) {
    family = names(fonts)[i]
    path = fonts_path[i]
    tryCatch({
      font_add(family, path)
    }, error = function(e) {
      message(sprintf('[%s]: %s', family, e$message))
    })
  }
}
