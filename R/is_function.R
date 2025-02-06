# #' @rdname XXX
#' @export
is_function <- function(x, lhs = NULL, ...) {
  storage.mode(x) == "function"
}
# #' @rdname XXX
#' @export

is_closure <- function(x) {
  typeof(x) == "closure"
}
# #' @rdname XXX
#' @export
is_primitive <- function(x) {
  is_function(x) && typeof(x) != "closure"
}
# #' @rdname XXX
#' @export
is_primitive_eager <- function(x) {
  typeof(x) == "builtin"
}
# #' @rdname XXX
#' @export
is_primitive_lazy <- function(x) {
  typeof(x) == "special"
}
# #' @rdname XXX
#' @export
is_lambda <- function(x) {
  inherits(x, "rlang_lambda_function")
}
