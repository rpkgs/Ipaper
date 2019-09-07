#' github
#' @export
github <- function(path = getwd()){
    cmd <- sprintf("github '%s'", path)
    system(cmd)
}

#' Attempts to install a package directly from gitee.
#' 
#' @inheritParams remotes::install_github
#' 
#' @examples
#' \dontrun{
#' install_gitee("kongdd/plyr")
#' install_gitee("kongdd/plyr")
#' }
#' @export
install_gitee <- function(repo){
    foreach(str = repo) %do% {
        url <- paste0("https://gitee.com/", str)
        install_git(url)    
    }
}

# #' @rdname install_gitee
# #' @importFrom remotes install_local install_git install_github
# #' @export
# install <- install_local

#' @rdname install_gitee
#' @export
install_github <- install_github
