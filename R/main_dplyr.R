#' @title data.frame manipulating function by `dplyr::across`
#' @name dt_tools
NULL

# ' @importFrom tidyselect where
#' @importFrom dplyr mutate across group_keys group_map
#' @importFrom tibble tibble
#' @rdname dt_tools
#' @export
dt_round <- function(d, digits = 2) {
  mutate(d, across(where(is.double), ~ round(.x, digits)))
}

#' @rdname dt_tools
#' @export
dt_chr2num <- function(d, fun = as.numeric) {
  # as.integer
  mutate(d, across(where(is.character), ~ fun(.x)))
}

# https://stackoverflow.com/questions/54774280/plyrddply-equivalent-in-dplyr


#' @importFrom data.table dcast
#' @export
dcast2 <- function(d, by, value.var = "value", ...) {
  vars_left <- setdiff(colnames(d), c(by, value.var)) %>% paste(collapse = "+")
  vars_right <- by %>% paste(collapse = "+")
  formula <- as.formula(sprintf("%s~%s", vars_left, vars_right))
  dcast(d, formula, value.var = value.var, ...)
}

#' @export
make_dt <- function(..., ncol = 3) {
  x <- list(...)
  n <- length(x)
  nrow <- floor(n / ncol)
  lapply(1:nrow, function(i) {
    ind <- seq((i - 1) * ncol + 1, i * ncol)
    x[ind] %>% as.data.table()
  }) %>% do.call(rbind, .)
}

#' @importFrom dplyr cur_group_id is_grouped_df mutate
#' @export
add_group_id <- function(d) {
  if (is_grouped_df(d)) {
    mutate(d, I = cur_group_id(), .before = 1)
  } else {
    mutate(d, I = 1:nrow(d), .before = 1)
  }
}

#' grouped_list
#'
#' @param data A list object, `length(data) = nrow(group)`, with elements in the
#' same order as group rows
#' @param group A data.frame or data.table object
#'
#' @export
grouped_list <- function(data, group) {
  R <- tibble(data, group)
  set_class(R, c("grouped_list", "tbl_df", "tbl", "data.frame"))
}

#' grouped_list
#'
#' @export
group_map2 <- function(df, .f, result.name = "data", ..., .keep = FALSE, .progress = FALSE) {
  group <- group_keys(df) %>% add_group_id()
  .f <- dplyr:::as_group_map_function(.f)

  if (.progress) {
    n <- nrow(group)
    pb <- make_progress(n)
    fun <- function(.x, .y, ...) {
      pb$tick() # 更新进度条
      .f(.x, .y, ...)
    }
  } else {
    fun <- .f
  }
  data <- group_map(df, fun, ..., .keep = .keep)
  R <- tibble(!!result.name := data, group)
  set_class(R, c("grouped_list", "tbl_df", "tbl", "data.frame"))
}


#' @export
export_fst <- function(x, ...) UseMethod("export_fst")

#' @importFrom fst write_fst read_fst
#' @export
export_fst.data.frame <- function(x, path, compress = 100, uniform_encoding = TRUE) {
  write_fst(x, path, compress, uniform_encoding)
}

#' @export
export_fst.grouped_list <- function(x, path, compress = 100, uniform_encoding = TRUE) {
  fcsv <- gsub(".fst", "_group.csv", path)
  fwrite(x[, -1], fcsv)

  df <- x[[1]] %>% melt_list("I") # first column is data
  write_fst(df, path, compress, uniform_encoding)
}


#' @export
import_fst <- function(
    path, columns = NULL, from = 1, to = NULL,
    as.data.table = TRUE, old_format = FALSE) {
  fcsv <- gsub(".fst", "_group.csv", path)
  data <- read_fst(path, columns, from, to, as.data.table, old_format)
  if (as.data.table) data = data.table(data)

  if (file.exists(fcsv)) {
    group <- fread(fcsv)
    lst <- split(select(data, -I), data$I)
    grouped_list(lst, group)
  } else {
    data
  }
}
