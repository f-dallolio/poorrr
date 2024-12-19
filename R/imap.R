#' Standalone `imap` - `purrr`-like functions
#'
#' @name imap
NULL
#' @rdname imap
#' @export
imap <- function(.x, .f, ...) {
  map2(.x = .x, names(.x) %||% seq_along(.x), .f, ...)
}
#' @rdname imap
#' @export
iwalk <- function(.x, .f, ...) {
  walk2(.x = .x, names(.x) %||% seq_along(.x), .f, ...)
}
#' @rdname imap
#' @export
imap_lgl <- function(.x, .y, .f, ...) {
  as.vector(imap(.x, .y, .f, ...), "logical")
}
#' @rdname imap
#' @export
imap_int <- function(.x, .y, .f, ...) {
  as.vector(imap(.x, .y, .f, ...), "integer")
}
#' @rdname imap
#' @export
imap_dbl <- function(.x, .y, .f, ...) {
  as.vector(imap(.x, .y, .f, ...), "double")
}
#' @rdname imap
#' @export
imap_chr <- function(.x, .y, .f, ...) {
  as.vector(imap(.x, .y, .f, ...), "character")
}
