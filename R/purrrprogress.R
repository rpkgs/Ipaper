# The following script is copied from:
#   https://github.com/TylerGrantSmith/purrrgress/blob/master/R/progressively.R.
# with the GPL3 license.
# Copywrite (c) 2019 Tyler Smith

#' @export
make_progress <- function(total = 100,
                          format = "(:current/:total) [:bar] :percent [Elapsed: :elapsed ETA: :eta]",
                          clear = FALSE,
                          show_after = 0,
                          width = getOption("width")) {
  progress::progress_bar$new(
    format = format,
    total = total,
    clear = clear,
    width = width,
    show_after = show_after
  )
}

.make_args <- function(args, .args, env) {
  mapper_type <- attr(.args, "type")
  n <- switch(mapper_type,
    .x = length(eval(args[[.args[[".xarg"]]]], envir = env)),
    .y = max(
      length(eval(args[[.args[[".xarg"]]]], envir = env)),
      length(eval(args[[.args[[".yarg"]]]], envir = env))
    ),
    .l = max(purrr::map_int(eval(args[[.args[[".larg"]]]], envir = env), length)),
    .if = sum(purrr:::probe(
      eval(args[[.args[[".xarg"]]]], envir = env),
      eval(args[[.args[[".parg"]]]], envir = env)
    )),
    .at = sum(purrr:::inv_which(
      eval(args[[.args[[".xarg"]]]], envir = env),
      eval(args[[.args[[".atarg"]]]], envir = env)
    ))
  )
  attr(args, "n") <- n
  return(args)
}

.pbify <- function(.mapper, .args) {
  pf <- function(...) {
    args <- .make_args(as.list(match.call())[-1], .args, rlang::caller_env())

    pb <- make_progress(attr(args, "n"))
    g <- eval(args[[.args[[".farg"]]]], rlang::caller_env())
    mod_f <- function(...) {
      .out <- purrr::as_mapper(g)(...)
      pb$tick()
      .out
    }
    args[[.args[[".farg"]]]] <- mod_f
    do.call(.mapper, args, envir = rlang::caller_env())
  }

  rlang::fn_fmls(pf) <- rlang::fn_fmls(.mapper)
  pf
}

.verify_args <- function(.mapper, args) {
  # checkmate::assert_function(.mapper)
  # checkmate::assert_subset(unlist(args, use.names = F), rlang::fn_fmls_names(.mapper))

  arg_names <- names(args)
  attr(args, "type") <-
    dplyr::case_when(
      ".atarg" %in% arg_names ~ ".at",
      ".parg" %in% arg_names ~ ".if",
      ".larg" %in% arg_names ~ ".l",
      ".y" %in% arg_names ~ ".y",
      TRUE ~ ".x"
    )
  return(args)
}


#' Helper function for generating progress bar functions
#' @export
progressively <- function(.mapper, .farg = NULL, .xarg = NULL, .yarg = NULL, .larg = NULL, .atarg = NULL, .parg = NULL) {
  args <- rlang::call_args(match.call())[-1]
  args <- .verify_args(.mapper, args)

  .pbify(.mapper, args)
}

#' Modified purrr functions with progress bar
#' @inheritParams purrr::map
#'
#' @export
pro_map <- progressively(purrr::map, .farg = ".f", .xarg = ".x")
