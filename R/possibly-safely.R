possibly <- function (.f, otherwise = NULL, quiet = TRUE) {
  .f <- as_function(.f)
  force(otherwise)
  check_bool(quiet)
  function(...) {
    tryCatch(.f(...), error = function(e) {
      # if (!quiet) message("Error: ", conditionMessage(e))
      # otherwise
      e
    })
  }
}
safely <- function (.f, otherwise = NULL, quiet = TRUE) {
  .f <- as_function(.f)
  force(otherwise)
  check_bool(quiet)
  function(...) capture_error(.f(...), otherwise, quiet)
}
capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
  tryCatch(list(result = code, error = NULL),
           error = function(e) {
             if (!quiet) message("Error: ", conditionMessage(e))
             list(result = otherwise, error = e)
           })
}
