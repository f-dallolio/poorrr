#' Find the value or position of the first match
#' @name detect
NULL

#' @rdname detect
#' @export
detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())
  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}

#' @rdname detect
#' @export
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())
  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}

.rlang_purrr_index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}


