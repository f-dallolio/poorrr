# #' @rdname XXX
#' @export
call_name <- function(call) {
  stopifnot(is.call(call))
  head <- call[[1]]
  if (is.call(head) && deparse(head[[1]]) %in% c("::", ":::")) {
    deparse(head[[3]])
  } else if (is.symbol(head)) {
    deparse(head)
  } else {
    NULL
  }
}
# #' @rdname XXX
#' @export
call_ns <- function(call) {
  stopifnot(is.call(call))
  head <- call[[1]]
  if (is.call(head) && deparse(head[[1]]) %in% c("::", ":::")) {
    deparse(head[[2]])
  } else {
    NULL
  }
}
# #' @rdname XXX
#' @export
call_args <- function(call) {
  stopifnot(is.call(call))
  as.list(call)[-1]
}

# #' @rdname XXX
#' @export
call_type <- function(x) {
  if (is_call(x, "~")) {
    stopifnot(length(x) == 2)
    x <- x[[2]]
  }
  stopifnot(typeof(x) == "language")
  type <- typeof(x[[1]])
  if (type == "symbol") {
    "named"
  } else if (is_namespaced_symbol(x[[1]])) {
    "namespaced"
  } else if (type == "language") {
    "recursive"
  } else if (type %in% c("closure", "builtin", "special")) {
    "inlined"
  } else {
    stop("corrupt language object")
  }
}
