#' @export
dt_mean <- function(d, by) {
  by <- substitute(by)
  d[, lapply(.SD, mean, na.rm = TRUE), by]
}

#' @importFrom rlang parse_exprs eval_bare expr
#' @export
get_yearly <- function(d, by = NULL) {
  by <- by %||% ""
  .by <- parse_exprs(by)
  by2 <- expr(list(year(date), !!!.by))

  expr <- expr(d[, lapply(.SD, mean, na.rm = TRUE), !!by2])
  # print(expr)
  rlang::eval_bare(expr)
}

#' @export
dt_day2mon <- function(dat, nmiss_day_per_mon = 3, ...) {
  # dat %<>% fix_uncontinue()
  dat_mon <- dat[, .(
    value = mean(value, na.rm = TRUE),
    # nmiss =  days_in_month(date[1]) - sum(!is.na(value))
    n_valid = sum(!is.na(value))
    # nmiss = sum(is.na(value))
  ), .(site, date = date_ym(date))]
  dat_mon %<>% mutate(n_miss = days_in_month(date) - n_valid)
  dat_mon
}

#' dt_day2year
#' @param dat A data.table, at least with the columns of `c("site", "date", "value")`
#' @export
dt_day2year <- function(
    dat,
    nmiss_day_per_mon = 3, nmiss_MonPerYear = 0, nmin_year = 55, ...)
{
  dat_mon <- dt_day2mon(dat, nmiss_day_per_mon)
  dat_year <- dat_mon[n_miss <= nmiss_day_per_mon, .(
    value = mean(value, na.rm = TRUE),
    n_miss = 12 - .N
  ), .(site, year(date))]

  ans <- dat_year[n_miss <= nmiss_MonPerYear, .(site, year, value)]
  # 最长的数据有62年，至少要有55年的数据
  info <- ans[, .N, site]
  ans <- merge(ans, info[N >= nmin_year, .(site)]) # yeraly data
  list(mon = dat_mon[, .(site, date, value)], year = ans)
}

# ' @param df_mon c("site", "date", "value")
#' @export
interp_hisavg_month <- function(df_mon) {
  df_mon %<>% mutate(month = month(date))
  d_his <- df_mon[, lapply(.SD, mean, na.rm = TRUE), .(site, month(date))] %>%
    rename(interp_mon = value)

  ans <- merge(df_mon, d_his, by = c("site", "month"), sort = FALSE)
  ans[is.na(value), value := interp_mon]
  ans %<>% setkeyv(c("site", "date"))
  ans[, .(site, date, value)]
}
