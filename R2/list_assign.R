#' Modify a list
#' @name list_assign
NULL

#' @rdname list_assign
#' @export
list_assign <- function(.x, ...) {
  check_list(.x)
  y <- dots_list(..., .named = NULL, .homonyms = "error")
  list_recurse(.x, y, function(x, y) y, recurse = FALSE, is_node = .is_node)
}

#' @rdname list_assign
#' @export
list_modify <- function(.x, ...) {
  check_list(.x)
  y <- dots_list(..., .named = NULL, .homonyms = "error")
  list_recurse(.x, y, function(x, y) y, is_node = .is_node)
}

#' @rdname list_assign
#' @export
list_merge <- function(.x, ...) {
  check_list(.x)
  y <- dots_list(..., .named = NULL, .homonyms = "error")
  list_recurse(.x, y, c, is_node = .is_node)
}

list_recurse <- function(x, y, base_f, recurse = TRUE, error_call = caller_env()) {
  is_node <- as_is_node(is_node, error_call = error_call, error_arg = ".is_node")
  if (!is_null(names(y)) && !is_named(y)) {
    cli::cli_abort("`...` arguments must be either all named or all unnamed.",
      call = error_call
    )
  }
  idx <- names(y) %||% rev(seq_along(y))
  for (i in idx) {
    x_i <- pluck(x, i)
    y_i <- pluck(y, i)
    if (is_zap(y_i)) {
      x[[i]] <- NULL
    } else if (recurse && is_node(x_i) && is_node(y_i)) {
      list_slice2(x, i) <- list_recurse(x_i, y_i, base_f)
    } else {
      list_slice2(x, i) <- base_f(x_i, y_i)
    }
  }
  x
}
`list_slice2<-` <- function(x, i, value) {
  if (is.null(value)) {
    x[i] <- list(NULL)
  } else {
    x[[i]] <- value
  }
  x
}


