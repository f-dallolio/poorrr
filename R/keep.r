#' Keep/discard elements based on their values
#' @name keep
NULL

#' @rdname keep
#' @export
keep <- function(.x, .f, ...) {
  .x[.purrr_probe(.x, .f, ...)]
}
#' @rdname keep
#' @export
discard <- function(.x, .p, ...) {
  sel <- .purrr_probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
#' @rdname keep
#' @export
compact <- function(.x) {
  Filter(length, .x)
}
