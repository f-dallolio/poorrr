#' Apply a function to each element of a vector conditionally
#' @name map_if
NULL

#' @rdname map_if
#' @export
map_if <- function(.x, .p, .f, ..., .else = NULL) {
  matches <- .purrr_probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  if (is.null(.else)) {
    return(.x)
  }
  .x[!matches] <- map(.x[!matches], .else)
  .x
}

.purrr_probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_function(.p, env = globalenv())
    map_lgl(.x, .p, ...)
  }
}
