#' obj.size
#' 
#' Get object size in `unit`
#' @param obj Object
#' @param unit "Kb", "Mb" or "Gb"
#' 
#' @examples
#' obj.size(1:100)
#' @export
obj.size <- function(obj, unit = "Mb"){
    cat(format(object.size(obj), unit), "\n")
}

#' file_size
#' 
#' @param file file path
#' @export
file_size <- function(file){
    utils:::format.object_size(file.size(file), "auto")
}
