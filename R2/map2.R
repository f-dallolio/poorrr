#' Map over two inputs
#' @name map2
NULL

#' @rdname map2
#' @export
map2 <- function(.x, .y, .f, ...) {
  .f <- as_function(.f, env = global_env())
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
  map2(.x, .y, .f, ...)
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

#' @rdname map2
#' @export
map2_vec <- function(.x, .y, .f, ...) {
  out <- map2(.x, .y, .f, ...)
  list_simplify(out, ptype = .ptype)
}



