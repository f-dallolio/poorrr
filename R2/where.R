where_at <- function(x, at, ..., error_arg = caller_arg(at), error_call = caller_env()) {
  if (is_formula(at)) {
    at <- as_function(at)
  }
  if (is.function(at)) {
    at <- at(names2(x))
  }
  if (is.numeric(at) || is.logical(at) || is.character(at)) {
    if (is.character(at)) {
      at <- intersect(at, names2(x))
    }
    loc <- vec_as_location(at, length(x), names2(x))
    seq_along(x) %in% loc
  } else {
    stop_input_type(
      x = at, "a numeric vector, character vector, or function.",
      arg = error_arg, call = error_call
    )
  }
}

where_if <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_predicate(.p, ...)
    map_lgl(.x, .p, ...)
  }
}