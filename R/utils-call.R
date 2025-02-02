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
