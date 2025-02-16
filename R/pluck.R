#' Safely get or set an element deep within a nested data structure
#' @name pluck
NULL

#' @rdname pluck
#' @export
`pluck<-` <- function(.x, ..., value) {
  assign_in(.x, list2(...), value)
}
#' @rdname pluck
#' @export
pluck <- function(.x, ..., .default = NULL) {
  .i <- list2(...)
  pluck_raw(.x = .x, .i = .i, .default = .default)
}
#' @rdname pluck
#' @export
pluck_exists <- function(.x, ...) {
  .i <- list2(...)
  nms <- names(.i)
  stopifnot(is.null(nms) || all(nms == ""))
  !is_zap(pluck_raw(.x, .i, .default = zap()))
}
#' @rdname pluck
#' @export
pluck_depth <- function(x, is_node = NULL) {
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
#' @rdname pluck
#' @export
ith <- function(x,
                i,
                default = NULL,
                arg = caller_arg(i),
                call = caller_env()) {
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
    n <- length(x)
    i <- ith_index(i,n, default)
    return(.subset2(x, i))
  }
  stop_input_type(
    x = i,
    what = "a string or a scalar numeric",
    arg = arg,
    call = call
  )
}
#' @rdname pluck
#' @export
ith_index <- function(i, n, .default = NULL) {
  if (i == 0 | i < -n | i > n) {
    return(.default)
  }
  if (i > 0) {
    return(i)
  }
  n + i + 1
}
pluck_raw <- function(.x, .i, .default = NULL) {
  x <- ith(x = .x, i = .i[[1]], default = .default)
  n <- length(.i)
  if (n > 1) {
    pluck_raw(.x = x, .i = .i[-1], .default = .default)
  } else {
    x
  }
}
