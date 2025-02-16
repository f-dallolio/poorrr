#' Find head/tail that all satisfies a predicate.
#' @name head_while
NULL

#' @rdname head_while
#' @export
head_while <- function(.x, .p, ...) {
  .p <- as_predicate(.p, ...)
  loc <- detect_index(.x, negate(.p), ...)
  if (loc == 0) {
    return(.x)
  }
  .x[seq_len(loc - 1)]
}
#' @rdname head_while
#' @export
tail_while <- function(.x, .p, ...) {
  .p <- as_predicate(.p, ...)
  loc <- detect_index(.x, negate(.p), ..., .right = TRUE)
  if (loc == 0) {
    return(.x)
  }
  .x[-seq_len(loc)]
}
