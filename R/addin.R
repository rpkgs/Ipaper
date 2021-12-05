#' Insert %in%.
#'
#' Call this function as an addin to insert %in% at the cursor position.
#' 
#' @keywords internal
#' @export
addin_insertIn <- function() {
  rstudioapi::insertText("%in% ")
}

#' @export
addin_insertDo <- function() {
  rstudioapi::insertText("%do% {}")
}

#' @export
addin_insertReturn <- function() {
  rstudioapi::insertText("%<>% ")
}

# fix new line ending in windows system
# If content is empty, not write
#' @import clipr
#' @export
write_clip2 <- function(content, ...){
    if (!is.null(content) && content != "") {
        if (.Platform$OS.type == "windows") {        
            writeLines(content, "clipboard", sep = "")
            # utils::writeClipboard(charToRaw(paste0(content, ' ')))    
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
    info <- getActiveDocumentContext()
    
    # set ranges
    rng <- info$selection[[1]]$range
    nline <- rng$end[1] - rng$start[1] + 1
    nchar <- rng$end[2] - rng$start[2] + 1
    
    if (nline == 1 & nchar == 1) {
        rng$start[2] <- 1
        rng$end[1]   <- rng$end[1] + 1 # row
        rng$end[2]   <- 1              # column
        # rng$end[2] <- Inf
        info$selection[[1]]$range <- rng
        info$selection[[1]]$text <- info$contents[rng$start[1]] %>% paste0("\n")
    }

    str <- info$selection[[1]]$text
    write_clip2(str, breaks = "") # only suit for windows
    if (output) return(info)
}

#' @export
addin_selectWord <- function() {
    info = rstudioapi::getActiveDocumentContext()
    rng <- info$selection[[1]]$range
    
    row = rng$start[1]
    col = rng$start[2]
    nline <- rng$end[1] - rng$start[1] + 1
    nchar <- rng$end[2] - rng$start[2] + 1

    # str_post = str_extract(row, glue("(?<=.{{col}})\\w+"))
    tryCatch({
        str = info$contents[row]
        pos = str_locate_all(str, "[\\w\\.]+") %>% 
            .[start <= col & end >= col, ]

        rng$start[2] = pos$start
        rng$end[2] = pos$end + 1
        info$selection[[1]]$range <- rng
        info$selection[[1]]$text <- info$contents[rng$start[1]]

        rstudioapi::setSelectionRanges(rng, info$id)
    }, error = function(e) {
        message(sprintf('%s', e$message))
    })
}

#' blind shortcuts to rstudio addin
#' @export
key_blind <- function(){
    # addins    
    # rstudio_bindings.json
    file_addin <- "~/.R/rstudio/keybindings/addins.json"
    indir <- dirname(file_addin)
    
    if (!dir.exists(indir)) mkdir(indir)
    if (!file.exists(file_addin)) writeLines("{}", file_addin)
    
    options_addin <- list(
        "Ipaper::addin_copyLines"      = "Alt+C",
        # "Ipaper::addin_cutLines"       = "Ctrl+X",
        "Ipaper::addin_insertDo"       = "Ctrl+Alt+D",
        "Ipaper::addin_selectWord"     = "Ctrl+D",
        "Ipaper::addin_insertIn"       = "Ctrl+Shift+I",
        "Ipaper::addin_insertReturn"   = "Ctrl+Shift+,",
        "Ipaper::smerge"         = "Ctrl+Shift+G", 
        "Ipaper::subl"           = "Alt+Shift+L", 
        "Ipaper::code"           = "Alt+Shift+C")
    
    file_rstudio <- "~/.R/rstudio/keybindings/rstudio_bindings.json"
    options_rstudio <- list(
        "commentUncomment" = "Ctrl+/",
        "executeCode"      = "Ctrl+R",
        "pasteLastYank"    = "Ctrl+Shift+Y"
    )
    
    options_update(file_addin, options_addin)
    options_update(file_rstudio, options_rstudio)   
}

#' @importFrom foreach %do% %dopar%
#' @importFrom jsonlite write_json read_json
#' @export
options_update <- function(file, options.new) {
    
    if (file.exists(file)) {
        options <- read_json(file)
    } else {
        mkdir(dirname(file))
        options <- list()
    }

    temp <- foreach (name = names(options.new), value = options.new) %do% {
        options[[name]] <- value
    }
    write_json(options, file, pretty = TRUE)
}
