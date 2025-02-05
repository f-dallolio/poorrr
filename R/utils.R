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

obj_is_vector <- function(x) {
  if (exists("vec_proxy", mode = "function")) {
    x <- vec_proxy(x)
    if (typeof(x) == "list") {
      return(TRUE)
    }
    return(obj_as_vector(x))
  }
  is_atomic(x) || obj_is_list(x) || is.data.frame(x)
}
list_all_vectors <- function(x) {
  all(vapply(x, obj_is_vector, logical(1)))
}

vec_size <- function(x) {
  if (is.data.frame(x)) {
    nrow(x)
  } else {
    length(x)
  }
}
list_sizes <- function(x, size = NULL) {
  stopifnot(obj_is_list(x))
  vapply(x, vec_size, integer(1))
}
list_all_size <- function(x, size, ..., .scalar_only = FALSE) {
  if (.scalar_only) {
    all(list_sizes(x) == as.integer(size))
  } else {
    all(list_sizes(x) %in% as.integer(size))
  }
}


is_mold <- function(type) {
  modes <- c("numeric", "logical", "integer", "double", "complex",
             "character", "raw")
  length(type) > 1 || (!type %in% modes)
}

is_mold("")



is_method_registered <- function (generic, class, env = parent.frame()) {
  fn <- get0(generic, envir = env)
  if (is.null(fn)) {
    return(FALSE)
  }
  tbl <- asNamespace(topenv(fn))$.__S3MethodsTable__.
  for (c in class) {
    name <- paste0(generic, ".", c)
    if (exists(name, envir = tbl, inherits = FALSE)) {
      return(TRUE)
    }
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
      return(TRUE)
    }
  }
  FALSE
}

typeof(lm(mpg ~ ., mtcars))
is_method_registered("vec_proxy", "POSIXlt")
vctrs:::vec_proxy.POSIXlt


can_simplify <- function(x, type = NULL) {
  is_atomic <- vapply(x, is.atomic, logical(1))
  if (!all(is_atomic)) return(FALSE)
  mode <- unique(vapply(x, typeof, character(1)))
  if (length(mode) > 1 && !all(c("double", "integer") %in% mode)) {
    return(FALSE)
  }

  # This can be coerced safely. If type is supplied, perform
  # additional check
  is.null(type) || can_coerce(x, type)
}



list_set_slice2 <- function(x, i, value) {
  if (is.null(value)) {
    x[i] <- list(NULL)
  } else {
    x[[i]] <- value
  }
  x
}

as_missing <- function(x) {
  if (is.data.frame(x)) {
    return(rep(NA, nrow(x)))
  }
  unlist(x[NA_integer_])
}


as_ith_location <- function (i, n, default = 0L) {
  if (is.character(n)) {
    names <- n
    n <- length(n)
  } else if (length(n) > 1) {
    n <- max(n)
  }

  ni <- length(i)
  if (is.logical(i)) {
    stopifnot(ni == as.integer(n))
    i <- which(i)[[1]]
    ni <- length(i)
  } else if (is.character(i)) {
    if (is.null(names)) {
      return(default)
    }
    stopifnot(ni == 1)
    i <- match(i, names, nomatch = 0)[[1]]
  } else if (is.numeric(i)) {
    stopifnot(ni == 1)
    i <- as.integer(i)
  }
  if (i > 0L && i <= n) {
    return(i)
  }
  if (i >= -n && i < 0L) {
    return(n + i + 1L)
  }
  default
}


pluck_i <- function(x, i, .default = NULL) {
  stopifnot(is_scalar_numeric(i) || is_string(i))
  if (is.character(i)) {
    if (i %in% names(x)) {
      return(x[[i]])
    }
    return(.default)
  }
  i <- as.integer(i)
  n <- length(x)
  if (i == 0 || i > n || i < -n) {
    return(.default)
  }
  if (i < 0) {
    i <- n + i + 1
  }
  x[[i]]
}
pluck_raw <- function(x, i, .default = NULL) {
  x <- pluck_i(x, c(i)[[1]], .default)
  if (length(i) == 1) {
    return(x)
  }
  pluck_raw(x, i[-1], .default)
}
pluck <- function(x, ..., .default = NULL) {
  pluck_raw(x, list(...), .default)
}

new_box <- function (.x, class = NULL, ...) {
  structure(list(.x), class = c(class, "rlang_box"), ...)
}
is_box <- function (x, class = NULL) {
  if(!inherits(x, "rlang_box")) {
    return(FALSE)
  }
  if (is.null(class)) {
    return(TRUE)
  }
  inherits(x, class)
}
as_box <- function (x, class = NULL) {
  if (is_box(x, class)) {
    x
  } else {
    new_box(x, class)
  }
}

iff <- \(x) {
  if (is.call(x) || is.symbol(x)) {
    if(is_call(x, "!") || is_call(x[[2]], "!") || is_call(x[[2]][[2]], "!")){
      out <- eval(x[[c(2,2,2)]])
    } else {
      out <- list(eval(x))
    }
    return(do.call(c, lapply(out, iff)))
  }
  if(inherits(x, "rlang_box_splice")) {
      return(x[[1]])
  }
  return(list(x))

}


ff <- function(...) {
  x <- do.call(c, lapply(eval(substitute(alist(...))), iff))
  vapply()
}


iff(x <- quote(!!!list(!!!mtcars, mtcars$mpg)))

