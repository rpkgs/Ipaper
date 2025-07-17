#' melt_list
#'
#' @param list A list object, with the same colnames data.frame in every element.
#' @param var.name vector of id variables. Can be integer (variable position)
#' or string (variable name). If blank, will use all non-measured variables.
#' @param na.rm Boolean
#' @param ... other parameters to melt
#'
#' @example R/examples/ex-melt_list.R
#'
#' @importFrom data.table melt is.data.table rbindlist
#' @references
#' 1. <https://stackoverflow.com/questions/15673550/why-is-rbindlist-better-than-rbind>
#' @export
melt_list <- function(list, ..., na.rm = TRUE, str2num = TRUE, str2factor = TRUE) {
  list <- rm_empty(list)
  if (is.null(list) || length(list) == 0) {
    return(NULL)
  }
  n <- length(list)

  params <- list(...)

  # check keys and values
  nkey <- length(params)
  l_vals <- rep(list(NULL), nkey)

  for (k in 1:nkey) {
    key <- names(params)[k]
    vals <- params[[k]]

    if (is.null(key)) {
      key <- vals # variable name
      vals <- names(list)
    }
    vals %<>% check_vals(n, str2num, str2factor)

    l_vals[[k]] <- vals
    names(l_vals)[k] <- key
  }

  first <- list[[1]]
  if (is.data.frame(first)) {
    for (i in seq_along(list)) {
      x <- list[[i]]

      for (k in 1:nkey) {
        vals <- l_vals[[k]]
        key <- names(l_vals)[k]
        eval(parse(text = sprintf("x$%s <- vals[i]", key)))
      }
      list[[i]] <- x
    }
    res <- rbindlist(list)
    # } else {
    #     id.vars <- colnames(first)
    #     res <- data.table::melt(list, ..., id.vars = id.vars, na.rm = na.rm)
    #     colnames(res) <- c(id.vars, keys)
  }
  keys <- names(l_vals)
  res %>% dplyr::relocate(all_of(keys))
}

# n: the number of variables
check_vals <- function(vals, n, str2num = TRUE, str2factor = TRUE) {
  if (is.null(vals)) vals <- 1:n
  if (length(vals) == 1) vals <- rep(vals, n)
  if (is.character(vals)) {
    if (str2num && is_num_char(vals)) {
      vals %<>% as.numeric()
    } else if (str2factor) {
      vals %<>% as.factor()
    }
  }
  vals
}

#' @rdname melt_list
#' @export
melt_tree <- function(x, names, ...) {
  first <- x[[1]]
  if (is.data.frame(first)) {
    if (length(names) > 1) {
      stop("length of `names` is not equal to the deep of list!")
    }
    melt_list(rm_empty(x), names[1], ...)
  } else if (is.list(first)) {
    # n <- length(names) # deeps of list
    # names2 <- names[1:(n-1)]
    map(x, melt_tree, names[-1]) %>% melt_list(names[1])
  } else {
    stop("Elements of x should be data.frame or list!")
  }
}

#' rm_empty
#' @param x A vector or list
#'
#' @examples
#' # numeric
#' x <- c(1:5, NA)
#' rm_empty(x)
#'
#' # list
#' l <- list(1:5, NULL, NA)
#' rm_empty(l)
#'
#' @keywords internal
#' @rdname tools
#' @export
rm_empty <- function(x) {
  if (is.list(x)) {
    x[!sapply(x, is_empty)]
  } else {
    x[!is.na(x)]
  }
}

#' @export
is_empty <- function(x) {
  is.null(x) || (is.data.frame(x) && nrow(x) == 0) || length(x) == 0
  # (is.numeric(x) && is.na(x))
}

empty <- is_empty
