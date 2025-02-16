as_is_node <- function(f,
                       error_call = caller_env(),
                       error_arg = caller_arg(f)) {
  if (is.null(f)) {
    obj_is_list
  } else {
    is_node_f <- as_function(f, call = error_call, arg = error_arg)
    as_predicate(is_node_f,
      .mapper = FALSE,
      .purrr_error_call = error_call,
      .purrr_error_arg = error_arg
    )
  }
}

as_predicate <- function(.fn, ...) {
  .fn <- as_function(.fn)
  function(...) {
    out <- .fn(...)
    if (!is_bool(out)) {
      abort(sprintf("Predicate functions must return a single `TRUE` or `FALSE`."))
    }
    out
  }
}
as_closure <- function(fn) {
  stopifnot(is.function(fn))
  if (typeof(fn) == "closure") {
    return(fn)
  }
  env <- environment(fn)
  fname <- fn_name(fn)
  n <- length(fname)
  name <- deparse(as.list(fname)[[n]])
  out <- args(fn)
  if (is.null(out)) {
    syms <- switch(name,
      "$" = alist(x = x, name = name),
      "(" = alist(x = x),
      ":" = alist(x = x, y = y),
      "=" = alist(x = x, y = y),
      "@" = alist(x = x, name = name),
      "[" = alist(x = x, i = i),
      "{" = alist(x = x),
      "~" = alist(x = x, y = y),
      "&&" = alist(x = x, y = y),
      "<-" = alist(name = name, value = value),
      "[[" = alist(x = x, i = i),
      "||" = alist(x = x, y = y),
      "[[<-" = alist(x = x, i = i, value = value),
      "$<-" = alist(x = x, name = name, value = value),
      "<<-" = alist(name = name, value = value),
      "@<-" = alist(x = x, name = name, value = value),
      "[<-" = alist(x = x, i = i, value = value),
      NULL
    )
    fmls <- rep(list(quote(expr = )), length(syms))
    names(fmls) <- names(syms)
    out <- as.function(
      c(
        fmls,
        call("{", as.call(c(as.symbol(name), unname(syms))))
      ),
      envir = topenv()
    )
    return(out)
  }
  nms <- names(formals(out))
  fmls <- lapply(nms, as.symbol)
  if (grepl("[^[:alnum:]._]", name)) {
    body_call <- as.call(c(as.symbol(name), fmls))
    body(out) <- call("{", body_call)
  } else {
    nms[nms == "..."] <- ""
    names(fmls) <- nms
    body(out) <- call("{", as.call(c(fname, fmls)))
  }
  out
}
fn_name <- function(fn, ..., no_ns = FALSE, str_out = FALSE) {
  env <- environment(fn)
  if (is.null(env)) {
    env <- asNamespace("base")
  }
  if (identical(globalenv(), env)) {
    env_name <- NULL
  } else {
    env_name <- environmentName(env)
  }
  fns <- mget(names(env),
    env,
    mode = "function",
    ifnotfound = list(NULL),
    inherits = FALSE
  )
  for (i in seq_along(fns)) {
    if (identical(fn, fns[[i]])) {
      nm <- names(fns)[[i]]
      if (nm == ".Last.value") {
        next
      }
      if (is.null(env_name) || no_ns) {
        if (str_out) {
          return(nm)
        }
        return(as.symbol(nm))
      } else {
        if (nm %in% getNamespaceExports(env)) {
          op <- "::"
        } else {
          op <- ":::"
        }
        out <- call(op, as.symbol(env_name), as.symbol(nm))
        if (str_out) {
          return(deparse(out))
        }
        return(out)
      }
    }
  }
  NULL
}
fn_names <- function(fns, ..., no_ns = FALSE, str_out = FALSE) {
  if (is.function(fns)) {
    fns <- list(fns)
  }
  obj_check_list(fns)
  stopifnot(all(map_lgl(fns, is.function)))

  out <- map(fns, fn_name, no_ns = FALSE, str_out = FALSE)
  names(out) <- map(out, ~ deparse(as.list(.x)[[length(.x)]]))
  out
}
fn_get_body <- function(x) {
  stopifnot(is.function(x))
  out <- body(as_closure(x))
  if (!is_call(out, "{")) {
    return(out)
  }
  as.list(out)[-1]
}
fn_set_body <- function(x, value) {
  if (is_call(value, "{")) {
    body(x) <- value
    return(x)
  }
  body(x) <- as.call(c(as.symbol("{"), value))
  x
}

list_names <- function(x, enframe = FALSE, unnest = FALSE) {
  out <- Filter(Negate(is.null), lapply(x, names))
  if (!enframe) {
    return(out)
  }
  nms <- names(out)
  if (is.null(nms)) {
    nms <- rep("", length(out))
  }
  if (unnest) {
    out <- data_frame(
      outer = rep(nms, lengths(out)),
      inner = do.call("c", unname(out))
    )
    class(out) <- c("tbl", class(out))
    return(out)
  }
  enframe(out)
}

is_deprecated <- function(fn) {
  if (!is.function(fn)) {
    return(FALSE)
  }
  x <- body(fn)
  if (is_call(x, "{")) {
    x <- x[[2]]
  }
  if (!is_call(x, ns = "lifecycle")) {
    return(FALSE)
  }
  out_name <- call_name(x) %in% c(
    "signal_stage",
    "deprecate_warn",
    "deprecate_stop",
    "deprecate_soft"
  )
  out_arg <- call_args(x)[[1]] %in% c("deprecated")
  out_name || out_arg
}

is_superseded <- function(fn) {
  if (!is.function(fn)) {
    return(FALSE)
  }
  x <- body(fn)
  if (is_call(x, "{")) {
    x <- x[[2]]
  }
  if (!is_call(x, ns = "lifecycle")) {
    return(FALSE)
  }
  out_name <- call_name(x) %in% c(
    "signal_superseded",
    "signal_stage"
  )
  out_arg <- call_args(x)[[1]] %in% c("superseded")
  out_name || out_arg
}

is_lifecycle <- function(fn, type = c("all", "deprecated", "superseded")) {
  switch(match.arg(type),
    all = is_superseded(fn) || is_deprecated(fn),
    deprecated = is_deprecated(fn),
    superseded = is_superseded(fn)
  )
}

no_zap <- function(x, error_call) {
  has_zap <- some(x, is_zap)
  if (!has_zap) {
    x
  } else {
    abort("Can't use `rlang::zap` to change the size of the output.")
  }
}
