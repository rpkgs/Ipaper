cmd_wsl = "/mnt/c/WINDOWS/system32/cmd.exe"

cmd_func <- function(command) {
    app = ""
    if (file.exists(cmd_wsl)) app = paste0(cmd_wsl, " /c ")

    function (path = getwd(), verbose = FALSE) {
        command %<>% path.mnt()
        path %<>% normalizePath()
        
        # command %<>% path.mnt()
        is_longname = !(basename(command) == command) # longname means windows 
        # fmt = ifelse(is_longname, '%s""%s" "%s""',  '%s%s "%s"')
        fmt = '%s%s "%s"'
        if (is_wsl() || is_win()) path %<>% win_path()
        
        if (is_longname) {
            app = ""
            fmt = '%s"%s" "%s"'
            if (is_win()) fmt = '%s""%s" "%s""'
        }
        cmd <- sprintf(fmt, app, command, path)
        if (verbose) cat(cmd, "\n")
        shell(cmd)
    }
}

shell <- function(..., ignore.stderr = FALSE, wait = FALSE){
    FUN <- switch(.Platform$OS.type, 
        "windows" = base::shell, 
        "unix" = base::system)
    suppressWarnings(FUN(..., ignore.stderr = ignore.stderr, wait = wait))
    invisible()
}

#' @title code editor
#' @name code_editor
#' 
#' @description sublime text3 ad vscode
#' 
#' @param verbose Boolean. Whether to print command into console?
#' 
#' @keywords internal
NULL

#' @rdname code_editor
#' @export
is_wsl <- function() file.exists("/mnt/c/WINDOWS/system32/cmd.exe")

#' @rdname code_editor
#' @export
is_win <- function() .Platform$OS.type == "windows"


#' @rdname code_editor
#' @export
OS_type <- function(){
    OS.type = .Platform$OS.type
    if (is_wsl()) OS.type = "wsl"
    OS.type
}

#' @rdname code_editor
#' @export
subl = cmd_func("subl")

#' @rdname code_editor
#' @export
code = cmd_func("code")
# code <- function(path = getwd(), verbose = FALSE) {
#     # path <- check_path(path)
#     cmd <- sprintf("/opt/bin/code '%s'", path)
#     if (verbose) print(cmd)
#     shell(cmd)
# }

#' @rdname code_editor
#' @export
smerge = cmd_func("smerge")

#' pdf_view
#' @param file the path of pdf file
#' 
#' @note not work in wsl
#' @export
pdf_view <- function(file, ...) {
    if (is_win() || is_wsl()) {
        SumatraPDF(file, ...)
    } else {
        evince(file, ...)
    }
}

#' @rdname pdf_view
#' @export
SumatraPDF <- cmd_func("C:/Program Files/RStudio/bin/sumatra/SumatraPDF.exe")

#' @rdname pdf_view
#' @export
evince <- cmd_func("evince")



#' Open directory in Explorer
#' 
#' @description open assign path in windows explorer, and default path is 
#' current directory. This function is only designed for windows system.
#' 
#' @param path the path you want to open
#' @export
dir.show <- function (path = getwd()) {
    if (!dir.exists(path)) path %<>% dirname()
    path <- check_path(path)
    
    cmd <- switch(OS_type(), 
        "windows" = paste("Explorer /e, ", path), 
        "unix" = sprintf("nautilus '%s'", path), 
        "wsl" = sprintf("%s /c Explorer /e, '%s'", cmd_wsl, path))
    shell(cmd)
}

# # display_tif <- function (data = NULL, file = NULL, width = NULL, height = NULL) 
# # IRdisplay:::display_raw("image/tif", TRUE, data, file, IRdisplay:::img_metadata(width,  height))
# #' jupyter display images
# #' 
# #' Only support jpeg, png, pdf and svg
# #' 
# #' @keywords internal
# #' @importFrom IRdisplay display_jpeg display_png display_pdf display_svg
# #' @export
# display <- function(file, width = NULL, height = NULL, ...) {
#     ext = tools::file_ext(file)
#     FUN <- get(sprintf("display_%s", ext))
#     FUN(file = file, width = width, height = height, ...)
# }

#' mkdir
#' 
#' @param path character vectors 
#' 
#' @importFrom foreach %do%
#' @export
mkdir <- function(path){
    for (path_i in unique(path)) {
       if (!dir.exists(path_i)) {
           dir.create(path_i, recursive = TRUE)
       }
    }
    path
}

#' @rdname mkdir
#' @export
check_dir <- mkdir

#' @keywords internal
#' @export
touch <- function(file) {
    mkdir(dirname(file))
    writeLines("", file)
}
