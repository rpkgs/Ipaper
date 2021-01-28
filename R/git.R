#' github
#' @export
github <- function(path = getwd()){
    cmd <- sprintf("github '%s'", path)
    system(cmd)
}

#' Attempts to install a package directly from gitee.
#' 
#' @param Repository address in the format `username/repo[/subdir][@ref|#pull]`. 
#' Alternatively, you can specify subdir and/or ref using the respective parameters (see below); 
#' if both is specified, the values in repo take precedence.
#' 
#' @examples
#' \dontrun{
#' install_gitee("adv-r/Ipaper")
#' install_gitee("adv-r/plyr")
#' }
#' @export
install_gitee <- function(repo){
    foreach(str = repo) %do% {
        url <- paste0("https://gitee.com/", str)
        install_git(url)    
    }
}

# #' @rdname install_gitee
# #' @export
# install <- install_local

#' @rdname install_gitee
#' @importFrom remotes install_git install_github
#' @export
install_github <- install_github

# #' @importFrom devtools document load_all
# #' @export
# load_all2 <- function(path = ".", ...){
#     document(path, ...)
#     load_all(path, ...)
# }
