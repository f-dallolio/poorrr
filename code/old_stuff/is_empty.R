# #' @rdname XXX
#' @export
is_empty <- function(x) {
  length(x) == 0
}
# #' @rdname XXX
#' @export
is_not_empty <- function(x) {
  !is_empty(x)
}

# #' @rdname XXX
#' @export
is_null <- function(x) {
  is.null(x)
}
# #' @rdname XXX
#' @export
is_not_null <- function(x) {
  !is_null(x)
}
