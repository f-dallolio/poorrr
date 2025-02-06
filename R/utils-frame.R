#' Get properties of the current or caller frame
#' @name utils-frame
NULL
#' @rdname utils-frame
#' @export
current_env <- function() {
  parent.frame()
}
#' @rdname utils-frame
#' @export
caller_env <- function(n = 1) {
  parent.frame(n + 1)
}
#' @rdname utils-frame
#' @export
caller_arg <- function (arg) {
  stopifnot(is.symbol(arg))
  arg <- substitute(arg)
  expr <- do.call(substitute, list(arg), envir = caller_env())
  deparse(expr)
}
