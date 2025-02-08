#' Tools for dealing with assignment calls (usually from parse)
#' @name utils-assn
NULL

#' @export
#' @rdname utils-assn
get_assn_obj <- function(x, env = globalenv()) {
  if (!.is_assn(x)) {
    return(x)
  }
  if (!.is_assn(x[[3]])) {
    out <- tryCatch(eval(x[[3]]), error = function(e) x[[3]])
    if (!is.null(environment(out))) {
      environment(out) <- env
    }
    return(out)
  }
  get_assn_obj(x[[3]])
}
#' @export
#' @rdname utils-assn
get_assn_name <- function(x) {
  if (!.is_assn(x)) {
    return(NULL)
  }
  nm <- deparse(x[[2]])
  if (!.is_assn(x[[3]])) {
    return(nm)
  }
  c(nm, sys.function()(x[[3]]))
}
.is_assn <- function(x, last = NULL) {
  is.call(x) && deparse(x[[1]]) == "<-"
}
.is_assn_last <- function(x) {
  .is_assn(x) && !.is_assn(x[[3]])
}
#' @export
#' @rdname utils-assn
eval_assn_call <- function(x, env = globalenv()) {
  if (is.list(x) || is.expression(x)) {
    out <- lapply(x, sys.function(), env)
    return(unlist(out))
  }
  nm <- get_assn_name(x)
  obj <- list(get_assn_obj(x, env = env))

  if (length(nm) > 1) {
    obj <- rep(obj, length(nm))
  }
  names(obj) <- nm
  obj
}
#' @export
#' @rdname utils-assn
deparse_assn_obj <- function(x, env = globalenv()) {
  deparse(get_assn_obj(x, env = env))
}
#' @export
#' @rdname utils-assn
deparse_assn_call <- function(x, as_string = FALSE, env = globalenv()) {
  out <- lapply(eval_assn_call(x, env = env), deparse)
  if (!as_string) {
    return(out)
  }
  sapply(out, paste0, collapse = "\n")
}
#' @export
#' @rdname utils-assn
string_assn_call <- function(x, ..., as_string = TRUE, env = globalenv()) {
  deparse_assn_call(x, as_string, env)
}
