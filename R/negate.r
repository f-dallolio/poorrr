#' Negate a predicate function so it selects what it previously rejected
#' @name negate
NULL

#' @rdname negate
#' @export
negate <- function(.p) {
  .p <- as_function(.p, env = globalenv())
  function(...) !.p(...)
}
