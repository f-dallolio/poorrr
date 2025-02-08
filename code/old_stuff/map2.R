#' Map over two inputs
#' @name map2
NULL

#' @rdname map2
#' @export
map2 <- function(.x, .y, .f, ...) {
  .f <- as_function(.f, env = globalenv())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}
#' @rdname map2
#' @export
walk2 <- function(.x, .y, .f, ...) {
  map2(.x = .x, .y = .y, .f = .f, ...)
  invisible(.x)
}

#' @rdname map2
#' @export
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
#' @rdname map2
#' @export
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
#' @rdname map2
#' @export
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
#' @rdname map2
#' @export
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
