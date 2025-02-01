#' Combine list elements into a single data structure
#' @name list_c
NULL

#' @rdname list_c
#' @export
list_c <- function(x) {
  do.call("c", x)
}
#' @rdname list_c
#' @export
list_rbind <- function(x) {
  do.call("rbind", x)
}
#' @rdname list_c
#' @export
list_cbind <- function(x) {
  do.call("cbind", x)
}
