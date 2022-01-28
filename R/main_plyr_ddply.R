
list_to_dataframe = plyr:::list_to_dataframe
loop_apply = plyr:::loop_apply
splitter_d = plyr:::splitter_d

#' @importFrom plyr . each progress_none create_progress_bar as.quoted 
#' quickdf rbind.fill.matrix
#' @export
plyr::.

make_names <- function(x, prefix = "X") {
    nm <- names(x)
    if (is.null(nm)) {
        nm <- rep.int("", length(x))
    }
    n <- sum(nm == "", na.rm = TRUE)
    nm[nm == ""] <- paste(prefix, seq_len(n), sep = "")
    nm
}

unrowname <- . %>% set_rownames(NULL)

#' Split `data.table`, apply function, and return results in a `data.table`.
#' 
#' For each subset of a `data.table`, apply function then combine results into a `data.table`.
#' 
#' @inheritParams plyr::ddply
#' @inheritParams purrr::map
#' @importFrom purrr as_mapper
#' 
#' @example R/examples/ex-ddply.R
#' @export
dt_ddply <- function(.data, .variables, .f = NULL, ..., .progress = "none",
                   .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL) {
    if (empty(.data)) {
        return(.data)
    }
    .f <- as_mapper(.f, ...)
    .variables <- as.quoted(.variables)
    pieces <- splitter_d(.data, .variables, drop = .drop)
    dt_ldply(
        .data = pieces, .fun = .f, ..., .progress = .progress,
        .inform = .inform, .parallel = .parallel, .paropts = .paropts
    )
}

#' @rdname dt_ddply
#' @export
dt_ldply <- function(.data, .fun = NULL, ..., .progress = "none", 
    .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA) {

     if (!inherits(.data, "split")) 
        .data <- as.list(.data)
    res <- llply2(.data = .data, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .parallel = .parallel, .paropts = .paropts)
    # res <- llply(
    #     .data = .data, .f = .fun, ..., .progress = .progress,
    #     .parallel = .parallel)
    if (identical(.id, NA)) {
        .id <- ".id"
        id_as_factor <- FALSE
    } else {
        id_as_factor <- TRUE
    }
    list_to_dataframe(res, attr(.data, "split_labels"), .id, id_as_factor)
}

#' @rdname dt_ddply
#' @export
dt_dlply <- function (.data, .variables, .fun = NULL, ..., .progress = "none", 
    .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL) 
{
    .variables <- as.quoted(.variables)
    pieces <- splitter_d(.data, .variables, drop = .drop)
    llply2(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .parallel = .parallel, .paropts = .paropts)
}

llply2 <- function (.data, .fun = NULL, ..., .progress = "none", 
    .inform = FALSE, .parallel = FALSE, .paropts = NULL) 
{
    if (is.null(.fun)) 
        return(as.list(.data))
    if (is.character(.fun) || is.list(.fun)) 
        .fun <- each(.fun)
    if (!is.function(.fun)) 
        stop(".fun is not a function.")
    if (!inherits(.data, "split")) {
        pieces <- as.list(.data)
        fast_path <- .progress == "none" && !.inform && 
            !.parallel
        if (fast_path) {
            return(structure(lapply(pieces, .fun, ...), dim = dim(pieces)))
        }
    } else {
        pieces <- .data
    }
    n <- length(pieces)
    if (n == 0) 
        return(list())
    if (.parallel && .progress != "none") {
        message("Progress disabled when using parallel plyr")
        .progress <- "none"
    }
    progress <- create_progress_bar(.progress)
    progress$init(n)
    on.exit(progress$term())
    result <- vector("list", n)
    # set_attr("vars", attr(pieces, "vars"))
    do.ply <- function(i) {
        # piece <- pieces[[i]]
        piece <- pieces$data[pieces$index[[i]], ] 
        if (.inform) {
            res <- try(.fun(piece, ...))
            if (inherits(res, "try-error")) {
                piece <- paste(utils::capture.output(print(piece)), collapse = "\n")
                stop("with piece ", i, ": \n", piece, call. = FALSE)
            }
        } else {
            res <- .fun(piece, ...)
        }
        progress$step()
        res
    }

    if (.parallel) {
        plyr:::setup_parallel()
        i <- seq_len(n)
        fe_call <- as.call(c(list(quote(foreach::foreach), i = i), 
            .paropts))
        fe <- eval(fe_call)
        result <- foreach::`%dopar%`(fe, do.ply(i))
    } else {
        result <- loop_apply(n, do.ply)
    }
    
    attributes(result)[c("split_type", "split_labels")] <- 
        attributes(pieces)[c("split_type", "split_labels")]
    names(result) <- names(pieces)
    if (!is.null(dim(pieces))) {
        dim(result) <- dim(pieces)
    }
    result
}

list_to_dataframe <- function (res, labels = NULL, id_name = NULL, id_as_factor = FALSE) {

    null <- vapply(res, is.null, logical(1))
    res <- res[!null]
    if (length(res) == 0) 
        return(data.frame())
    if (!is.null(labels)) {
        stopifnot(nrow(labels) == length(null))
        labels <- labels[!null, , drop = FALSE]
    }
    names_res <- names(res)
    if (!is.null(id_name) && is.null(labels) && !is.null(names_res)) {
        stopifnot(length(id_name) == 1)
        if (id_as_factor) 
            names_res <- factor(names_res, levels = unique(names_res))
        labels <- data.frame(.id = names_res, stringsAsFactors = FALSE)
        names(labels) <- id_name
    }
    atomic <- unlist(lapply(res, is.atomic))
    df <- unlist(lapply(res, is.data.frame))
    mat <- unlist(lapply(res, is.matrix))

    if (all(mat)) {
        resdf <- as.data.frame(rbind.fill.matrix(res))
        rows <- unlist(lapply(res, NROW))
    } else if (all(atomic)) {
        nrow <- length(res)
        ncol <- unique(unlist(lapply(res, length)))
        if (length(ncol) != 1) 
            stop("Results do not have equal lengths")
        vec <- unname(do.call("c", res))
        resdf <- quickdf(unname(split(vec, rep(seq_len(ncol), 
            nrow))))
        names(resdf) <- make_names(res[[1]], "V")
        rows <- rep(1, length(nrow))
    } else if (all(df)) {
        # browser()
        # resdf <- rbind.fill(res)
        resdf <- do.call(rbind, res)
        rows <- unlist(lapply(res, NROW))
    } else {
        stop("Results must be all atomic, or all data frames")
    }
    if (is.null(labels)) 
        return(unrowname(resdf))
    names(labels) <- make_names(labels, "X")
    cols <- setdiff(names(labels), names(resdf))
    
    labels <- labels[rep(1:nrow(labels), rows), cols, drop = FALSE]
    if (is_empty(cols)) set_rownames(resdf, NULL) else cbind(data.table(labels), resdf)
}
