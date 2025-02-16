#' Accumulate intermediate results of a vector reduction
#' @name accumulate
NULL

#' @rdname accumulate
#' @export
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}



