#' Apply a function to each element of a vector
#' @name map
NULL

#' @rdname map
#' @export
map <- function(.x, .f, ...) {
  .f <- as_function(.f)
  lapply(.x, .f, ...)
}

#' @rdname map
#' @export
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

#' @rdname map
#' @export
map_lgl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, logical(1), ...)
}

#' @rdname map
#' @export
map_int <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, integer(1), ...)
}

#' @rdname map
#' @export
map_dbl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, double(1), ...)
}

#' @rdname map
#' @export
map_chr <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, character(1), ...)
}

#' @rdname map
#' @export
map_vec <- function(.x, .f, ...) {
  out <- map(.x, .f, ...)
  list_simplify0(out)
}

.rlang_purrr_map_mold <- function(.x, .f, .mold, ...) {
  .f <- as_function(.f, env = global_env())
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}


