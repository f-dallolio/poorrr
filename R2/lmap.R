#' Apply a function to list-elements of a list
#' @name lmap
NULL

#' @rdname lmap
#' @export
lmap <- function(.x, .f, ...) {
  lmap_helper(.x, rep(TRUE, length(.x)), .f, ...)
}

#' @rdname lmap
#' @export
lmap_if <- function(.x, .p, .f, ...) {
  where <- where_if(.x, .p)
  lmap_helper(.x, where, .f, ..., .else = .else)
}

#' @rdname lmap
#' @export
lmap_at <- function(.x, .at, .f, ...) {
  where <- where_at(.x, .at)
  lmap_helper(.x, where, .f, ...)
}

lmap_helper <- function(.x, .ind, .f, ..., .purrr_error_call = caller_env()) {
  .f <- as_function(.f)
  if (!is.null(.else)) {
    .else <- as_function(.else)
  }
  out <- vector("list", length(.x))
  for (i in seq_along(.x)) {
    if (.ind[[i]]) {
      res <- .f(.x[i], ...)
    } else if (is.null(.else)) {
      res <- .x[i]
    } else {
      res <- .else(.x[i], ...)
    }
    if (!is.list(res)) {
      msg <- sprintf(
        ".f(.x[[%i]])) must return a list, not %s",
        i, obj_type_friendly(res)
      )
      abort(msg)
    }
    out[[i]] <- res
  }
  if (is.data.frame(.x)) {
    out <- lapply(out, as.data.frame)
    list_cbind(out)
  } else {
    list_flatten(out)
  }
}


