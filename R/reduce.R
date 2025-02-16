#' Reduce a list to a single value by iteratively applying a binary function
#' @name reduce
NULL

#' @rdname reduce
#' @export
reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
#' @rdname reduce
#' @export
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
