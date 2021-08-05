#' @export
foreach <- function(..., .combine, .init, .final=NULL, .inorder=TRUE,
                    .multicombine=FALSE,
                    .maxcombine=if (.multicombine) 100 else 2,
                    .errorhandling=c('stop', 'remove', 'pass'),
                    .packages=NULL, .export=NULL, .noexport=NULL,
                    .verbose=FALSE) 
{
  .final <- set_names_foreach(.final, .combine, ...)

  if (missing(.combine)) {
    if (!missing(.init))
      stop('if .init is specified, then .combine must also be specified')
    .combine <- defcombine
    hasInit <- TRUE
    init <- quote(list())
  } else {
    .combine <- match.fun(.combine)
    if (missing(.init)) {
      hasInit <- FALSE
      init <- NULL
    } else {
      hasInit <- TRUE
      init <- substitute(.init)
    }
  }

  # .multicombine defaults to TRUE if the .combine function is known to
  # take multiple arguments
  if (missing(.multicombine) &&
      (identical(.combine, cbind) ||
       identical(.combine, rbind) ||
       identical(.combine, c) ||
       identical(.combine, defcombine)))
    .multicombine <- TRUE

  # sanity check the arguments
  if (!is.null(.final) && !is.function(.final))
    stop('.final must be a function')
  if (!is.logical(.inorder) || length(.inorder) > 1)
    stop('.inorder must be a logical value')
  if (!is.logical(.multicombine) || length(.multicombine) > 1)
    stop('.multicombine must be a logical value')
  if (!is.numeric(.maxcombine) || length(.maxcombine) > 1 || .maxcombine < 2)
    stop('.maxcombine must be a numeric value >= 2')
  if (!is.character(.errorhandling))
    stop('.errorhandling must be a character string')
  if (!is.null(.packages) && !is.character(.packages))
    stop('.packages must be a character vector')
  if (!is.null(.export) && !is.character(.export))
    stop('.export must be a character vector')
  if (!is.null(.noexport) && !is.character(.noexport))
    stop('.noexport must be a character vector')
  if (!is.logical(.verbose) || length(.verbose) > 1)
    stop('.verbose must be a logical value')

  specified <- c('errorHandling', 'verbose')
  specified <- specified[c(!missing(.errorhandling), !missing(.verbose))]

  args <- substitute(list(...))[-1]

  if (length(args) == 0)
    stop('no iteration arguments specified')
  argnames <- names(args)
  if (is.null(argnames))
    argnames <- rep('', length(args))

  # check for backend-specific options
  options <- list()
  opts <- grep('^\\.options\\.[A-Za-z][A-Za-z]*$', argnames)
  if (length(opts) > 0) {
    # put the specified options objects into the options list
    for (i in opts) {
      bname <- substr(argnames[i], 10, 100)
      options[[bname]] <- list(...)[[i]]
    }

    # remove the specified options objects from args and argnames
    args <- args[-opts]
    argnames <- argnames[-opts]
  }

  # check for arguments that start with a '.', and issue an error,
  # assuming that these are misspelled options
  unrecog <- grep('^\\.', argnames)
  if (length(unrecog) > 0)
    stop(sprintf('unrecognized argument(s): %s',
                 paste(argnames[unrecog], collapse=', ')))

  # check for use of old-style arguments, and issue an error
  oldargs <- c('COMBINE', 'INIT', 'INORDER', 'MULTICOMBINE', 'MAXCOMBINE',
               'ERRORHANDLING', 'PACKAGES', 'VERBOSE', 'EXPORT', 'NOEXPORT',
               'LOADFACTOR', 'CHUNKSIZE')
  oldused <- argnames %in% oldargs
  if (any(oldused))
    stop(sprintf('old style argument(s) specified: %s',
                 paste(argnames[oldused], collapse=', ')))

  .errorhandling <- match.arg(.errorhandling)

  combineInfo <- list(fun=.combine, in.order=.inorder, has.init=hasInit,
                      init=init, final=.final, multi.combine=.multicombine,
                      max.combine=.maxcombine)
  iterable <- list(args=args, argnames=argnames, evalenv=parent.frame(),
                   specified=specified, combineInfo=combineInfo,
                   errorHandling=.errorhandling, packages=.packages,
                   export=.export, noexport=.noexport, options=options,
                   verbose=.verbose)
  class(iterable) <- 'foreach'
  iterable
}

# ' `.final` function, set names of `foreach` objection.
# '
# ' Automatically inherit names of list or data.table, and rm the rownames of
# ' matrix.
# '
# ' @examples
# ' \dontrun{
# ' .final <- set_names_foreach(.final, .combine)
# ' }
set_names_foreach <- function(.final, .combine, ...){
    if (is.null(.final)) {
        if (missing(.combine)) {
            # for data.frame and list
            y   <- list(...)[[1]]
            y_class <- class(y)
            y_names <- names(y)

            if (!('iter' %in% y_class) && !is.null(y_names) &&
                ( is.list(y) || is.vector(y)) )
            {
                .final <- function(x) { setNames(x, y_names[1:length(x)]) }
            }
        } else {
            # 2. for matrix
            FUN_combine <- match.fun(.combine)
            if (identical(FUN_combine, rbind)) {
                # .final <- function(mat) { `rownames<-`(mat, NULL) }
                ## can't fixed the error of R 3.6.0
                .final = function(mat) {
                    mat2 <- if (is.null(colnames(mat))) {
                        `dimnames<-`(mat, NULL)
                        # `attributes<-`(mat, attributes(mat)[1])
                    } else {
                        `rownames<-`(mat, NULL)
                    }
                    as.matrix(mat2)
                }
            }
        }
    }
    .final
}

defcombine <- function(a, ...) c(a, list(...))
