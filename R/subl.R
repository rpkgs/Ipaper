#' subl
#' @export
subl <- function (path = getwd()) 
{
    path <- normalizePath(path)
    cmd <- sprintf('subl "%s"', path)
    # if (.Platform$OS.type == "windows"){
    #     path <- gsub("/", "\\", path)
    # }
    # print(path)
    # print(cmd)
    system(cmd)
}



#' github
#' @export
github <- function(path = getwd()){
    cmd <- sprintf("github '%s'", path)
    system(cmd)
}
