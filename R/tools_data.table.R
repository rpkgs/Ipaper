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
#' @importFrom data.table CJ
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

  # 获取每个维度的唯一值（保持原顺序）
  dimnames <- lapply(dt[, ..dim_cols], function(x) unique(x))

  # 创建 array 并用 aperm 调整维度顺序
  dims <- lengths(dimnames) # 计算维度大小
  dim = rev(dims) %>% setNames(NULL)
  arr <- array(dt[[value_col]], dim = dim, dimnames = rev(dimnames))
  aperm(arr)
}
