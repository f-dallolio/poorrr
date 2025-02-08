# #' @rdname XXX
#' @export
is_call <- function(x, name = NULL, n = NULL, ns = NULL) {
  if (!is.call(x)) {
    return(FALSE)
  }
  if (!is.null(name) && !call_name(x) %in% name) {
    return(FALSE)
  }
  if (!is.null(n) && !length(call_args(x)) %in% n) {
    return(FALSE)
  }
  if (!is.null(ns) && !call_ns(x) %in% ns) {
    return(FALSE)
  }
  TRUE
}
# #' @rdname XXX
#' @export
is_call_simple <- function(x) {
  head <- x[[1]]
  if (is.symbol(head)) {
    return(TRUE)
  }
  is.call(head) && deparse(head) %in% c("::", ":::")
}

is_namespaced_symbol <- function(x, ns = NULL, private = NULL) {
  if (typeof(x) != "language") {
    return(FALSE)
  }
  if (!is.null(ns) && !identical(x[[2]], as.symbol(ns))) {
    return(FALSE)
  }
  head <- x[[1]]
  if (is.null(private)) {
    identical(head, quote(`::`)) || identical(head, quote(`:::`))
  } else if (private) {
    identical(head, quote(`:::`))
  } else {
    identical(head, quote(`::`))
  }
}
is_ns_sym <- function(x, ns = NULL, private = NULL) {
  is_namespaced_symbol(x = x, ns = ns, private = private)
}

is_namespaced_call <- function(x, ns = NULL, private = NULL) {
  if (typeof(x) != "language") {
    return(FALSE)
  }
  if (!is_namespaced_symbol(x[[1]], ns, private)) {
    return(FALSE)
  }
  TRUE
}
is_ns_call <- function(x, ns = NULL, private = NULL) {
  is_namespaced_call(x = x, ns = ns, private = private)
}
