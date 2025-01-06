call_name <- function (call) {
  stopifnot(!identical(call, quote(expr = )))
  if (inherits(call, "quosure") || is_call(call, name = "~")) {
    call <- unclass(as.list(call)[-1])
  }
  stopifnot(is_call(call))
  if (is_call(call, c("::", ":::"))) return(NULL)
  head <- call_head(call)
  if(is.symbol(head)) return(deparse(head))
  deparse(head[[3]])
}
call_set_name <- function(call, value, ..., .keep_ns = FALSE) {
  out <- call_set_head(call, value)
  if(.keep_ns) call_ns(out) <- call_ns(call)
  out
}


call_head <- function(call) {
  call[[1]]
}
call_set_head <- function(call, value) {
  if(is_string(value)) value <- tryCatch(str2lang(value), error = \(e) as.symbol(e))
  call <- as.list(call)
  call[[1]] <- value
  as.call(call)
}
`call_head<-` <- function(call, value) {
  call_set_head(call, value)
}

call_head_name <- function(call) {
  deparse(call_head(call))
}

call_args <- function(x) {
  stopifnot(is.call(x))
  as.list(x)[-1]
}
call_set_args <- function(call, value) {
  if(!is.list(value)) value = list(value)
  as.call(c(call_head(call), value))
}
`call_args<-` <- function(call, value) {
  call_set_args(call, value)
}


call_args_names <- function(x) {
  names(call_args(x))
}
call_ns <- function(call) {
  if(!is_call_simple(call) || !is_namespaced_call(call)) return(NULL)
  deparse(call_head(call)[[2]])
}
`call_ns<-` <- function(call, value) {
  call_set_ns(call, value, .private = FALSE)
}
call_set_ns <- function(call, value, ..., .private = FALSE){
  if(is_string(value)) value <- tryCatch(str2lang(value), error = \(e) as.symbol(e))
  head <- call(if(.private) ":::" else "::",
               value,
               as.symbol(call_name(call)))
  as.call(c(head, call_args(call)))
}


call2 <- function (.fn, ..., .ns = NULL, .private = FALSE) {
  if(is_string(.fn)) .fn <- as.symbol(.fn)
  if(!is.null(.ns)) {
    if(isNamespace(ns)) .ns <- gsub("^.*[:]", "", environmentName(ns))
    if(is_string(.ns)) .ns <- as.symbol(ns)
    .fn <- call(if(.private) ":::" else "::", .fn, .ns)
  }
  as.call(c(.fn, list(...)))
}

