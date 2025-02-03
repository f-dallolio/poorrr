# fn_name ----
fn_name <- function(fn, env = NULL) {
  if (!is.null(env) && is.f) {
    if (is_string(env)) {
      env <- asNamespace(env)
    }
    return(.fn_name_closure(fn, env))
  }
  switch(typeof(fn),
    "builtin" = ,
    "special" = .fn_name_primitive(fn),
    "closure" = .fn_name_closure(fn),
    stop("Argument `fn` is not a function.")
  )
}
.fn_name_closure <- function(fn, env = NULL) {
  if (is.null(env)) {
    env <- environment(fn) %||% "base"
  }
  if (is_string(env)) {
    if (env %in% c("R_GlobalEnv", "global")) {
      env <- globalenv()
    } else {
      env <- asNamespace(env)
    }
  }
  nms <- names(env)
  fns <- mget(nms[nms != ".Last.value"],
              env,
              mode = "function",
              inherits = FALSE,
              ifnotfound = list(NULL))
  fns <- Filter(length, fns)
  for (i in seq_along(fns)) {
    if (identical(fn, fns[[i]])) {
      return(names(fns)[[i]])
    }
  }
  NULL
}
.fn_name_primitive <- function(fn, env = NULL) {
  if (!is.null(env)) {
    .fn_name_closure(fn, env = env)
  }
  str2lang(deparse(fn))[[2]]
}

# fn_match ----
fn_match <- function(x, env = NULL, strict = FALSE, quiet = FALSE) {
  if(is.function(x)) {
    return(x)
  }
  switch(typeof(x),
         character = .fn_match_character(x, env, strict, quiet),
         symbol = .fn_match_symbol(x, env, strict, quiet),
         language = .fn_match_call(x, env, strict, quiet),
         stop("Cannot match input to a function."))
}
.fn_match_character <- function (x, env = NULL, strict = FALSE, quiet = FALSE) {
  stopifnot(length(x) == 1)
  if (grepl(":{2,3}", x)) {
    return(eval(str2lang(x)))
  }
  if (is.null(env)) {
    env <- parent.frame(2)
  }
  if (strict) {
    return(get(x, mode = "function", envir = env))
  }
  out <- get0(x, mode = "any", envir = env)
  if (is.function(out)) {
    return(out)
  }
  if (quiet) {
    return(NULL)
  }
  if (is.null(out)) {
    warning("Returning `NULL`. Cannot find a function called `", x, "`.")
  } else {
    class <- attr(out, "class")
    if (is.null(class)) {
      msg <- sprintf("Found non-function \"%s\" (mode: %s).", x, mode(out))
    } else {
      msg <- sprintf("Found non-function \"%s\" of class `%s`(mode: `%s`).", x, class[[1]], mode(out))
    }
    warning("Returning `NULL`. ", msg)
  }
  NULL
}
.fn_match_symbol <- function (x, env = NULL, strict = FALSE, quiet = FALSE) {
  if (is.null(env)) {
    env <- parent.frame(2)
  }
  fn_match(deparse(x), env = env, strict = strict, quiet = quiet)
}
.fn_match_call <- function (x, env = NULL, strict = FALSE, quiet = FALSE) {
  if (identical(x[[1]], quote(`~`))) {
    env <- environment(x)
    x <- x[[length(x)]]
    if (!is.call(x)) {
      return(fn_match(x))
    }
  }
  head <- x[[1]]
  if (is.function(head)){
    return(head)
  }
  if (deparse(head) %in% c("::", ":::")) {
    return(eval(x))
  }
  fn_match(head, env, strict, quiet)
}



fn_match(quote(rlang:::call_add_namespace()))
