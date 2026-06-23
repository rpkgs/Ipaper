#' fread_dir
#' 
#' @param ... others to [data.table::fread()]
#' 
#' @importFrom data.table rbindlist data.table
#' @export
fread_dir <- function(indir, pattern = "*.csv", ..., .progress="text", list2df=FALSE) {
  fs = dir(indir, pattern, full.names = TRUE)
  fs = set_names(fs, gsub(".csv", "", basename(fs)))

  res = llply(fs, fread, .progress=.progress, ...)
  if (!list2df) return(res)

  tryCatch({
    rbindlist(res)
  }, error = function(e) {
    message(sprintf('%s', e$message))
    res
  })
}


#' @export
fwrite2 <- function(x, file) {
  write.table(x, file, sep = ",", row.names = FALSE, fileEncoding = "gbk")
}


#' Convert array to data.table
#' @param arr input array
#' @param dimnames list of dimension names, e.g., `dimnames(arr)`
#' @return data.table
#' 
#' @example R/examples/array2dt.R
#' 
#' @importFrom data.table CJ chmatch
#' @export
array2dt <- function(arr, dimnames) {
  # 注意顺序要匹配，aperm非常必要!
  do.call(CJ, c(dimnames, sorted = FALSE)) %>%
    cbind(value = c(aperm(arr)))
}

#' @rdname array2dt
#' @export
dt2array <- function(dt, value_col = "value") {
  # 获取维度列（除了值列之外的所有列）
  dim_cols <- setdiff(names(dt), value_col)

  # 获取每个维度的唯一值（保持首次出现的顺序）
  dimnames <- lapply(dt[, ..dim_cols], unique_sort)
  dims <- setNames(lengths(dimnames), NULL)

  # 按维度取值定位填充，不依赖 dt 的行顺序；缺失组合记为 NA
  # 字符列用 data.table::chmatch (比 match 快约 5 倍)，其余用 match
  .match <- function(x, levs) if (is.character(x)) chmatch(x, levs) else match(x, levs)
  idx <- mapply(function(col, levs) .match(dt[[col]], levs), dim_cols, dimnames)
  arr <- array(dt[[value_col]][NA_integer_], dim = dims, dimnames = dimnames)
  arr[idx] <- dt[[value_col]]
  arr
}
