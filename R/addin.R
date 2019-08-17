#' Insert \%in\%.
#'
#' Call this function as an addin to insert \code{ \%in\% } at the cursor position.
#' 
#' @keywords internal
#' @export
addin_insertIn <- function() {
  rstudioapi::insertText(" %in% ")
}

#' @export
addin_insertDo <- function() {
  rstudioapi::insertText(" %do% {}")
}

#' @export
addin_insertReturn <- function() {
  rstudioapi::insertText(" %<>% ")
}

# fix new line ending in windows system
# If content is empty, not write
#' @import clipr
#' @export
write_clip2 <- function(content, ...){
    if (!is.null(content) && content != "") {
        if (.Platform$OS.type == "windows") {        
            utils::writeClipboard(charToRaw(paste0(content, ' ')))    
        } else {
            write_clip(content, ...)
        } 
    }
}

#' Cut lines as sublime
#' 
#' @param output Boolean, whether return selection info?
#' 
#' @keywords internal
#' @export
#' @importFrom rstudioapi modifyRange getSourceEditorContext getActiveDocumentContext
#' @import clipr
addin_cutLines <- function() {
    info <- addin_copyLines(output = TRUE)
    modifyRange(info$selection[[1]]$range, "", info$id)
}

#' @rdname addin_cutLines
#' @export
addin_copyLines <- function(output = FALSE){
    # info <- getSourceEditorContext()
    info <- getActiveDocumentContext()
    
    # set ranges
    rng <- info$selection[[1]]$range
    nline <- rng$end[1] - rng$start[1] + 1
    nchar <- rng$end[2] - rng$start[2] + 1
    if (nline == 1 & nchar == 1) {
        rng$start[2] <- 1
        rng$end[2] <- Inf
        info$selection[[1]]$range <- rng
        info$selection[[1]]$text <- info$contents[rng$start[1]]
    }

    str <- info$selection[[1]]$text
    write_clip2(str, breaks = "") # only suit for windows
    if (output) return(info)
}

#' blind shortcuts to rstudio addin
key_blind <- function(){
    # addins    
    # rstudio_bindings.json
    file_addin <- "~/.R/rstudio/keybindings/addins.json"
    options_addin <- list(
        "Ipaper::addin_copyLines"      = "Alt+C",
        "Ipaper::addin_cutLines"       = "Alt+X",
        "Ipaper::addin_insertDo"       = "Ctrl+Alt+D",
        "Ipaper::addin_insertIn"       = "Ctrl+Shift+I",
        "Ipaper::addin_insertReturn"   = "Ctrl+Shift+,")
    
    file_rstudio <- "~/.R/rstudio/keybindings/rstudio_bindings.json"
    options_rstudio <- list(
        "commentUncomment" = "Ctrl+/",
        "executeCode"      = "Ctrl+R",
        "pasteLastYank"    = "Ctrl+Shift+Y"
    )
    
    options_update(file_addin, options_addin)
    options_update(file_rstudio, options_rstudio)   
}

#' @importFrom foreach foreach %do%
#' @importFrom jsonlite write_json read_json
#' @export
options_update <- function(file, options.new) {
    
    if (file.exists(file)) {
        options <- read_json(file)
    } else {
        check_dir(basename(file))
        options <- list()
    }

    temp <- foreach (name = names(options.new), value = options.new) %do% {
        options[[name]] <- value
    }
    write_json(options, file)
}

# Name: Insert %in%
# Description: Inserts `%in%` at the cursor position.
# Binding: addin_insertIn
# Interactive: false

# Name: Insert %do%
# Description: Inserts `%do%` at the cursor position.
# Binding: addin_insertDo
# Interactive: false

# Name: Insert %<>%
# Description: Inserts `%<>%` at the cursor position.
# Binding: addin_insertReturn
# Interactive: false

# Name: Cut lines
# Description: Cut lines at the cursor position.
# Binding: addin_cutLines
# Interactive: false

# Name: Copy lines
# Description: Copy lines at the cursor position.
# Binding: addin_copyLines
# Interactive: false
