#' par_sbatch
#'
#' R parallel function, designed for linux sbatch model.
#'
#' @param X a vector (atomic or list) or an expressions vector. Other objects
#' (including classed objects) will be coerced by as.list.
#' @param FUN the function to be applied to (mclapply) each element of X or
#' (mcmapply) in parallel to ....
#' @param ... other parameters passed to mclapply
#' @param return.res Default is no return result.
#' @param Save save result in rda file or not?
#' @param outdir String, output directory for rda file if Save = true.
#' 
#' @importFrom parallel mclapply
#' @export
par_sbatch <- function(X, FUN, ..., return.res = F, Save = F, outdir = '.'){
    nparams <- length(X)
    # NODES, CPUS_PER_NODE are defined in sbatch file
    # READ NODES AND CPUS FROM SYSTEM ENV
  
    # @param nodes The (maximum) number of cluster nodes to spread the calculation
    # over. slurm_apply automatically divides params in chunks of approximately
    # equal size to send to each node. Less nodes are allocated if the parameter
    # set is too small to use all CPUs on the requested nodes.
    # @param cpus_per_node The number of CPUs per node on the cluster;
    # determines how many processes are run in parallel per node.
    I_node        <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) #node ID
    nodes         <- as.numeric(Sys.getenv('SLURM_JOB_NUM_NODES'))
    cpus_per_node <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
    
    if (is.na(I_node)) I_node <- 0 # sinteractive mode
    
    if (nparams < cpus_per_node * nodes) {
        nchunk <- cpus_per_node
    }else {
        nchunk <- ceiling(nparams/nodes) # one CPUS more than one tasks
    }

    # nodes <- ceiling(nparams/nchunk)
    I_beg  <- I_node * nchunk + 1
    I_end  <- min((I_node + 1) * nchunk, nparams)
    if (I_beg > I_end){
        fprintf('It is empty in this node!')
        return(NULL)
    }
    # cl  <- parallel::makeCluster(cpus_per_node, type = "FORK") #fork only work in linux
    # print(length(cl))

    # If error, still can close cluster works
    # https://stat.ethz.ch/R-manual/R-devel/library/parallel/html/mclapply.html
    res <- tryCatch({
        ## parallel function for parLapplyLB
        # parLapplyLB(cl, sites[I_beg:I_end], calsite_pheno, df = df) #mcmapply,
        print(names(X[I_beg:I_end])) 
        mclapply(X[I_beg:I_end], FUN, mc.cores = cpus_per_node, ...)  #SIMPLIFY
    }, error = function(e){
        message(sprintf('[error]: %s', e$message))
        # stopCluster(cl)
    })
    # , warning = function(w){
    #     message(sprintf('[warning]: %s', w$message))
    #     # stopCluster(cl)
    # } #can't write warning here, code break
    if (Save){
        if (!dir.exists(outdir)) dir.create(outdir)
        saveRDS(res, file = paste0(outdir, '/results_', I_node, '.RDS'))        
    }
    # stopCluster(cl)
    if (return.res) return(res)
    NULL 
}

#' get_sbatch
#' 
#' Merge the slurm result.
#' 
#' @param indir directory to search rda files
#' @param pattern an optional regular expression. Only file names which match 
#' the regular expression will be returned.
#' @param Save save result in rda file or not?
#' @param outfile If Save = T, output rda file name.
#' 
#' @export
get_sbatch <- function(indir = '.', pattern = 'result.*.RDS',
                          Save = TRUE, outfile = "result.rda"){
    files <- dir(indir, pattern, full.names = T)
    cat('OUTPUTs:', "\n")
    print(basename(files))
    RES <- lapply(files, readRDS) %>% do.call(c, .)

    if (Save) save(RES, file = outfile)
    return(RES)
}
