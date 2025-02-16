#' Apply a function to each element of a vector conditionally
#' @name map_if
NULL

#' @rdname map_if
#' @export
map_if <- function(.x, .p, .f, ...) {
  matches <- .rlang_purrr_probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}
#' @rdname map_if
#' @export
map_at <- function(.x, .at, .f, ...) {
  where <- where_at(.x, .at)
  out <- vector("list", length(.x))
  out[where] <- map(.x[where], .f, ...)
  out[!where] <- .x[!where]
  setNames(out, names(.x))
}
.rlang_purrr_probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_function(.p, env = global_env())
    map_lgl(.x, .p, ...)
  }
}
