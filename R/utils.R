as_function <- function(x, env = globalenv(), ...) {
  if (nargs()) {
    stopifnot(...length() == 0)
  }
  if (is.function(x)) {
    return(x)
  }
  if (inherits(x, "quosure")) {
    stop("Cannot handle `quosures`. Install and use `rlang::as_function()`.")
  }
  if (is.call(x) && deparse(x[[1]]) == "~") {
    if (length(x) > 2) {
      stop("Cannot coerce a two-sided formula to a function.")
    }
    env <- attr(x, ".Environment")
    if (!is.environment(env)) {
      stop("Formula must carry an environment.")
    }
    args <- alist(... = , .x = ..1, .y = ..2, . = ..1)
    fn <- eval(call("function", as.pairlist(args), x[[2]]))
    environment(fn) <- env
    class(fn) <-  c("rlang_lambda_function", "function")
    return(fn)
  }
  if (is.character(x) && length(x) == 1) {
    return(get(x, envir = env, mode = "function"))
  }
  stop("Cannot coeerce `x` into a function")
}


as_predicate <- function (.fn, ..., .allow_na = FALSE) {
  .fn <- as_function(.fn)
  function(...) {
    out <- .fn(...)
    if (!is_bool(out)) {
      if (is_na(out) && .allow_na) {
        return(NA)
      }
      stop("A predicate function must return a single `TRUE` or `FALSE`.")
    }
    out
  }
}


recycle <- function(x, n) {
  if (is.null(x) || is.null(n)) {
    return(NULL)
  }
  n_x <- length(x)
  if (n_x == n) {
    return(x)
  }
  if (n == 0L) {
    return(rep(x, 0))
  }
  if (n_x == 1L) {
    rep(x, n)
  } else {
    stop("Incompatible lengths: ", n_x, ", ", size, call. = FALSE)
  }
}

set_names <- function(x, nm = x, ...) {
  n <- length(x)
  if (is.null(nm)) {
    return(unname(x))
  }
  stopifnot(length(nm) %in% c(1, n))
  if (n == 1) {
    nm <- rep(nm, n)
  }
  names(x) <- nm
  x
}
paste_line0 <- function(x, .trailing = TRUE) {
  if (.trailing) {
    paste0(x, "\n", collapse = "")
  } else {
    paste(x, collapse = "\n")
  }
}

paste_line <- function(..., .trailing = TRUE) {
  text <- as.character(list(...))
  if (.trailing) {
    paste0(text, "\n", collapse = "")
  } else {
    paste(text, collapse = "\n")
  }
}

obj_is_list <- function(x) {
  is_bare_list(x) || inherits(x, "list")
}

`list_slice2<-` <-  function (x, i, value) {
  if (is.null(value)) {
    x[i] <- list(NULL)
  }
  else {
    x[[i]] <- value
  }
  x
}
list_set_slice2 <- function(x, i, value) {
  if (is.null(value)) {
    x[i] <- list(NULL)
  } else {
    x[[i]] <- value
  }
  x
}


list_set_slice2(as.list(letters), 2, NULL)

x <- as.list(letters)
x[[2]] <- NULL
x
x <- mtcars
row.names(x) <- NULL
x
