#' Convert an object into a mapper function
#' @export
as_mapper <- function (.f, ...) {
  UseMethod("as_mapper")
}
#' @export
as_mapper.default <- function (.f, ...) {
  if (typeof(.f) %in% c("special", "builtin")) {
    .f <- as_closure(.f)
    if (identical(environment(.f), asNamespace("base"))) {
      environment(.f) <- globalenv()
    }
    return(.f)
  }
  as_function(.f)
}
#' @export
as_mapper.character <- function(.f, ...) {
  stopifnot(length(.f) == 1)
  as.function(c(alist(.f = , "..." = ), call("pluck", x, .f)))
}
#' @export
as_mapper.numeric <- function(.f, ...) {
  stopifnot(length(.f) == 1)
  as.function(c(alist(.f = , "..." = ), call("pluck", x, .f)))
}
#' @export
as_mapper.list <- function(.f, ...) {
  stopifnot(length(.f) == 1)
  as.function(c(alist(.f = , "..." = ), call("pluck", x, .f)))
}
