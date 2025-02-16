#' Modify a pluck location
#' @name modify_in
NULL

#' @rdname modify_in
#' @export
modify_in <- function(.x, .where, .f, ...) {
  .where <- as.list(.where)
  .f <- as_function(.f)
  value <- .f(pluck(.x, !!!.where), ...)
  assign_in(.x, .where, value)
}

#' @rdname modify_in
#' @export
assign_in <- function(x, where, value) {
  n <- length(where)
  if (n == 0) {
    abort("Argument `where` must contain at least one element.")
  } else if (n > 1) {
    old <- pluck(x, where[[1]], .default = list())
    if (!is_zap(value) || !identical(old, list())) {
      value <- assign_in(old, where[-1], value)
    }
  }
  if (is_zap(value)) {
    x[[where[[1]]]] <- NULL
  } else {
    list_slice2(x, where[[1]]) <- value
  }
  x
}



