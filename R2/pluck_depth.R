#' Compute the depth of a vector
#' @name pluck_depth
NULL

#' @rdname pluck_depth
#' @export
pluck_depth <- function(x) {
  if (is.null(is_node)) {
    is_node <- function(x) is.expression(x) || is.list(x)
  }
  is_node <- as_is_node(is_node)
  if (is_node(x)) {
    depths <- map_int(x, pluck_depth, is_node = is_node)
    1L + max(depths, 0L)
  } else if (is_atomic(x)) {
    1L
  } else {
    0L
  }
}

pluck_raw <- function(.x, .i) {
  x <- ith(x = .x, i = .i[[1]], default = .default)
  n <- length(.i)
  if (n > 1) {
    pluck_raw(.x = x, .i = .i[-1], .default = .default)
  } else {
    x
  }
}
ith <- function(x, i, arg = caller_arg(i), call = caller_env()) {
  if (is.language(x) || is.function(x)) {
    x <- as.list(x)
  }
  if (is_string(i)) {
    i <- match(i, names(x))
    if (is.na(i)) {
      return(default)
    }
    return(.subset2(x, i))
  }
  if (is.numeric(i) && length(i) == 1) {
    i <- as.integer(i)
    n <- length(x)
    if (i == 0 || i < -n || i > n) {
      return(default)
    }
    if (i < 0) {
      i <- n + i + 1
    }
    return(.subset2(x, i))
  }
  stop_input_type(
    x = i, what = "a string or a scalar numeric",
    arg = arg, call = call
  )
}


