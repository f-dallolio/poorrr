#' Keep/discard elements based on their values
#' @name keep
NULL

#' @rdname keep
#' @export
keep <- function(.x, .f, ...) {
  .x[.rlang_purrr_probe(.x, .f, ...)]
}

#' @rdname keep
#' @export
discard <- function(.x, .p, ...) {
  sel <- .rlang_purrr_probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}

#' @rdname keep
#' @export
compact <- function(.x) {
  .x[as.logical(lengths(.x))]
}



