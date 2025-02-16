#' Combine list elements into a single data structure
#' @name list_c
NULL

#' @rdname list_c
#' @export
list_rbind <- function(x) {
  obj_check_list(x)
  do.call("rbind", x)
}

#' @rdname list_c
#' @export
list_cbind <- function(x) {
  obj_check_list(x)
  do.call("cbind", x)
}

#' @rdname list_c
#' @export
list_c <- function(x) {
  do.call("c", x)
}



