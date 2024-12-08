call_name <- function(x) {
  stopifnot(is.call(x))
  deparse(x[[1]])
}
call_args <- function(x) {
  stopifnot(is.call(x))
  as.list(x)[-1]
}
call_args_names <- function(x) {
  names(call_args(x))
}



global_env <- function() {
  globalenv()
}

names2 <- function(x) {
  nms <- names(x)
  if(is.null(nms)) return(setNames(x, rep("", length(x))))
  nms[is.na(nms)] <- ""
  nms
}
`names2<-` <- function(x, value) {
  value[is.na(value)] <- ""
  setNames(x, value)
}
set_names <- function(x, nm = x, ...) {
  names2(x) <- nm
  x
}

vec_as_location <- function(i, n, names = vector("", n)) {
  out <- seq_len(n)
  names(out) <- names

  # Special-case recycling to size 0
  if (is_logical(i, n = 1) && !length(out)) {
    return(out)
  }

  unname(out[i])
}

as_function <- function (x, env = global_env()) {
  if (is.function(x)) return(x)
  if (is.call(x) && identical(x[[1]], quote(`~`))) {
    stopifnot("If a formula, `x` must be one-sided (rhs)." = length(x) < 3)
    env <- attr(x, ".Environment")
    stopifnot(is.environment(env))
    args <- list(... = quote(expr = ),
                 .x = quote(..1),
                 .y = quote(..2),
                 . = quote(..1))
    fn <- structure(.Data = as.function(c(args, x[[2]]), envir = env),
                    class = c("rlang_lambda_function", "function"))
    return(fn)
  }
  if (is_string(x)) {
    return(get(x, envir = env, mode = "function"))
  }
  stop("Can't convert `x` to a function.")
}


is_atomic <- function(x, n = NULL) {
  if(length(n) && length(x) != n) return(FALSE)
  is.atomic(x) && length(x) > 0
}

is_list <- function(x, n = NULL) {
  if(length(n) && length(x) != n) return(FALSE)
  is.list()
}

is_vector <- function(x, n = NULL) {
  is_atomic(x, n) || is_list(x, n)
}

is_scalar <- function(x) {
  is_vector(x, n = 1)
}

is_bool <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_string <- function(x, string = NULL) {
  if(length(x) > 1 || !is.character(x)) return(FALSE)
  if(is.null(string)) return(TRUE)
  x == string
}


is_true <- function(x) {
  identical(x, TRUE)
}
is_false <- function(x) {
  identical(x, FALSE)
}

is_formula <- function(x, scoped = NULL, lhs = NULL) {
  if(is.call(x) && identical(x[[1]], quote(`~`))) return(FALSE)
  inherits(x, "formula")
  is.environment(attr(x, ".Environment"))
}

zap <- function() {
  structure(list(), class = "rlang_zap")
}

is_zap <- function(x) {
  inherits(x, "rlang_zap")
}

has_zap <- function(x, negate = FALSE) {
  out <- any(vapply(x, is_zap, TRUE))
  if(negate) return(!out)
  out
}

