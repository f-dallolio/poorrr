#' Modify a pluck location
#' @name modify_in
NULL
#' @rdname modify_in
#' @export
assign_in <- function (x, where, value) {
  n <- length(where)
  if (n == 0) {
    stop("Argument `where` must contain at least one element.")
  }
  else if (n > 1) {
    old <- tryCatch(x[[where[[1]]]], error = function(e) list())
    if (!inherits(value, "rlang_zap") || !identical(old, list())) {
      value <- assign_in(old, where[-1], value)
    }
  }
  if (inherits(value, "rlang_zap")) {
    x[[where[[1]]]] <- NULL
  }
  else {
    list_slice2(x, where[[1]]) <- value
  }
  x
}
