#' subl
#' @export
subl <- function (path = getwd()) 
{
    cmd <- sprintf("subl '%s'", path)
    system(cmd)
}


#' github
#' @export
github <- function(path = getwd()){
    cmd <- sprintf("github '%s'", path)
    system(cmd)
}
