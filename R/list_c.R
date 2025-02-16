#' Combine list elements into a single data structure
#' @name list_c
NULL

#' @name list_c
#' @export
list_c <- function(x) {
  inject(c(!!!x))
}
#' @name list_c
#' @export
list_rbind <- function(x) {
  obj_check_list(x)
  do.call("rbind", x)
}
#' @name list_c
#' @export
list_cbind <- function(x) {
  obj_check_list(x)
  do.call("cbind", x)
}
