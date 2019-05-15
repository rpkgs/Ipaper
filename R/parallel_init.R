#' killCluster
#' 
#' @export
killCluster <- function(){
    os = .Platform$OS.type
    if (os == "windows") {
        system("taskkill /IM Rscript.exe -f")
    } else if (os == "unix"){
        system("pkill -f R")
        # NULL
    }
}

#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster
#' @export
InitCluster <- function(ncluster = 4, outfile = "log.txt"){
    # file_log <- "outfile.txt"
    if (file.exists(outfile)) file.remove(outfile)
    cl <- makeCluster(ncluster, outfile = outfile)
    registerDoParallel(cl)
}

# #' @import doFuture 
# #' @importFrom future makeClusterPSOCK plan
# #' @export
# InitCluster2 <- function(ncluster = 4, outfile = "log.txt"){
#     # file_log <- "outfile.txt"
#     if (file.exists(outfile)) file.remove(outfile)

#     registerDoFuture()

#     cl <- makeClusterPSOCK(ncluster, outfile = outfile)
#     plan(cluster, workers = cl)
# }
