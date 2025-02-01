#' @export
as_call <- function(x) {
  UseMethod("as_call")
}
#' @export
as.call.default <- function(x) {
  as.call(x)
}
#' @export
as_call.function <- function(x) {
  stopifnot(typeof(x) == "closure")
  body <- body(x)
  if (!(is.call(body) && deparse(body[[1]]) == "{")) {
    body <- call("{", body)
  }
  call("function", as.pairlist(formals(x)), body)
}
#' @export
as_call.name <- function(x) {
  as.call(list(x))
}
#' @export
as_call.character <- function(x) {
  if (length(x) == 1) {
    return(call(x))
  }
  stop("Input must be a string.")
}
#' @export
as_call.list <- function(x) {
  if (is.character(x[[1]])) {
    x[[1]] <- tryCatch(str2lang(x[[1]]), error = function(e) as.symbol(x))
  }
  as.call(x)
}
#' @export
as_call.pairlist <- function(x) {
  as_call.list(as.list(x))
}
