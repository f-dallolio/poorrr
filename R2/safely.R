#' Wrap a function to capture errors
#' @name safely
NULL

#' @rdname safely
#' @export
safely <- function(.f) {
  .f <- as_function(.f)
  force(otherwise)
  function(...) capture_error(.f(...), otherwise)
}

capture_error <- function(code) {
  tryCatch(list(result = code, error = NULL), error = function(e) {
    list(
      result = otherwise,
      error = e
    )
  })
}


