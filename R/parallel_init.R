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
InitCluster <- function (ncluster = 4, outfile = "log.txt", kill = TRUE) 
{
    if (kill) killCluster()
    if (file.exists(outfile)) file.remove(outfile)
    
    if (.Platform$OS.type == "unix") {
        doMC::registerDoMC(ncluster)
    } else {
        cl <<- parallel::makeCluster(ncluster, outfile = outfile)
        doParallel::registerDoParallel(cl)
    }
}

#' @export
gc_cluster <- function(n){
    # if (is.null(n)) {
    #     n <- length(foreach:::.foreachGlobals$data)
    # }
    ok(sprintf("[gc] %d clusters ...\n", n))
    
    temp <- foreach(i = seq_len(n)) %dopar% {
        rm(list = ls())
        gc(); gc()
    }
}

# For bigmemory crash
#' @export
gc_linux <- function(){
    if (.Platform$OS.type == "unix") {
        system('rm /dev/shm -r', ignore.stderr = TRUE)
    }
}

shell <- function(...){
    FUN <- switch(.Platform$OS.type, 
        "windows" = base::shell, 
        "unix" = base::system)
    FUN(...)
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
