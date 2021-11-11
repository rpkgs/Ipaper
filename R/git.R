#' @title git tools
#' @name git_tools
NULL

#' @rdname git_tools
#' @export
git_push <- function(f = FALSE) {
    cmd = ifelse(f, "git push -f", "git push")
    system(cmd)
}

#' @rdname git_tools
#' @export
git_commit_amend <- function() {
    system("git add -A")
    system("git commit --amend --no-verify --no-edit")
}

#' @rdname git_tools
#' @export
git_commit <- function(title = Sys.time()) {
    system("git add -A")
    system(sprintf('git commit -a -m "%s" --no-edit', title))
}

#' @rdname git_tools
#' @export
git_set_remote <- function(url) {
    cmd = sprintf("git remote set-url origin %s", url)
    system(cmd)
}


#' github
#' @export
github <- function(path = getwd()) {
    cmd <- sprintf("github '%s'", path)
    system(cmd)
}

# #' @rdname install_gitee
# #' @export
# install <- install_local

# #' @importFrom devtools document load_all
# #' @export
# load_all2 <- function(path = ".", ...){
#     document(path, ...)
#     load_all(path, ...)
# }

#' Attempts to install a package directly from gitee.
#'
#' @param Repository address in the format `username/repo[/subdir][@ref|#pull]`.
#' Alternatively, you can specify subdir and/or ref using the respective parameters (see below);
#' if both is specified, the values in repo take precedence.
#'
#' @examples
#' \dontrun{
#' install_gitee("adv-r/Ipaper")
#' }
#' @export
install_gitee <- function(repo) {
    foreach(str = repo) %do% {
        url <- paste0("https://gitee.com/", str)
        install_git(url)
    }
}
