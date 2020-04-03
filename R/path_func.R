#' convert windows path to wsl
#'
#' In windows system, conversion will not be applied.
#' @inheritParams base::normalizePath
#'
#' @export
path.mnt <- function(path) {
    sep = substr(path, 2, 2)

    if (is_wsl()) {
        if (sep == ":") {
            pan  = substr(path, 1, 1) %>% tolower()
            path = sprintf("/mnt/%s/%s", pan, substr(path, 4, nchar(path)))
        }
    }
    path
}

win_path <- function(path, winslash = "\\\\"){
    if (substr(path, 1, 4) == "/mnt") {
        pan = substr(path, 6, 6)
        path = paste0(pan, ":", substr(path, 7, nchar(path)))
    }
    gsub("/", winslash, path)
}

check_path <- function(path) {
    path = normalizePath(path)
    if (OS_type() %in% c("wsl", "windows")) {
        path <- win_path(path)
    }
    path
}

#' @export
file_ext <- function(file) {
    str_extract(basename(file), "(?<=\\.).{1,4}$") 
}

#' @importFrom stringr str_locate_all
#' @export
file_name <- function(file) {
    file  <- basename(file)
    pos   <- str_locate_all(file, "\\.")[[1]] 
    I_dot <- pos[nrow(pos), 1]
    
    # no dot
    if (length(I_dot) == 0) return(file)
    
    postfix = substr(file, I_dot, nchar(file))
    if (nchar(postfix) <= 5) {
        gsub(postfix, "", file)
    } else {
        file
    }
    # gsub("\\..*", "", basename(file))
    # str_extract(basename(file), ".*(?=\\.?)")
    # ?: zero or one
    # *: zero or more
    # +:  one or more
}

#' edit R profile by sublime
#' 
#' @export
edit_r_profile_sys <- function(){
    code(file.path(R.home(), "etc")) # /Rprofile.site
}
