#' Wrap a function to return a value instead of an error
#' @name possibly
NULL

#' @rdname possibly
#' @export
possibly <- function(.f) {
  .f <- as_function(.f)
  force(otherwise)
  function(...) {
    tryCatch(.f(...), error = function(e) e)
  }
}



