#' settting font family for plot
#' 
#' @export
#' @keywords internal
#' @export 
set_font <- function() {
    OS.type = .Platform$OS.type
	if (OS.type == 'windows') {
	    grDevices::windowsFonts(
	        Times = grDevices::windowsFont("Times New Roman"), 
	        Arial = grDevices::windowsFont("Arial"), 
            hwfsong = grDevices::windowsFont("STSong"),
            song  = grDevices::windowsFont("SimSun"),
            fsong = grDevices::windowsFont("FangSong"),
			rTimes = grDevices::windowsFont("rTimes"),
			TimesSimSun = grDevices::windowsFont("TimesSimSun"),
	        YH = grDevices::windowsFont("Microsoft Yahei"), 
	        whit = grDevices::windowsFont("Whitney-Book")
	    )
	} else if (OS.type == 'unix'){
	    Cairo::CairoFonts(
	        regular="Times New Roman:style=Regular",
	        bold="Times New Roman:style=Bold",
	        italic="Times New Roman:style=Oblique",
	        bolditalic="Times New Roman:style=BoldOblique"
	    )
	}
}
