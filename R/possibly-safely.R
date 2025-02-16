possibly <- function(.f, otherwise = NULL) {
  .f <- as_function(.f)
  force(otherwise)
  function(...) {
    tryCatch(.f(...), error = function(e) e)
  }
}

safely <- function(.f, otherwise = NULL) {
  .f <- as_function(.f)
  force(otherwise)
  function(...) capture_error(.f(...), otherwise)
}
capture_error <- function(code, otherwise = NULL) {
  tryCatch(list(result = code, error = NULL),
    error = function(e) list(result = otherwise, error = e)
  )
}
