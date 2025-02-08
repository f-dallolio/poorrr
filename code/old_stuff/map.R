#' Apply a function to each element of a vector
#' @name map
NULL

#' @rdname map
#' @export
map <- function(.x, .f, ...) {
  .f <- as_function(.f, env = globalenv())
  lapply(.x, .f, ...)
}
#' @rdname map
#' @export
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}
#' @rdname map
#' @export
map_lgl <- function(.x, .f, ...) {
  .purrr_map_mold(.x, .f, logical(1), ...)
}
#' @rdname map
#' @export
map_int <- function(.x, .f, ...) {
  .purrr_map_mold(.x, .f, integer(1), ...)
}
#' @rdname map
#' @export
map_dbl <- function(.x, .f, ...) {
  .purrr_map_mold(.x, .f, double(1), ...)
}
#' @rdname map
#' @export
map_chr <- function(.x, .f, ...) {
  .purrr_map_mold(.x, .f, character(1), ...)
}
.purrr_map_mold <- function(.x, .f, .mold, ...) {
  .f <- as_function(.f, env = globalenv())
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}
