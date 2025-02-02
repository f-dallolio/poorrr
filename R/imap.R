#' Apply a function to each element of a vector, and its index
#' @name imap
NULL

#' @rdname imap
#' @export
imap <- function(.x, .f, ...) {
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}
#' @rdname imap
#' @export
iwalk <- function(.x, .f, ...) {
  imap(.x = .x, .f = .f, ...)
  invisible(.x)
}
#' @rdname imap
#' @export
imap_lgl <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "logical")
}
#' @rdname imap
#' @export
imap_int <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "integer")
}
#' @rdname imap
#' @export
imap_dbl <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "double")
}
#' @rdname imap
#' @export
imap_chr <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "character")
}
