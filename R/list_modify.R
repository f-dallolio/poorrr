list_assign <- function(.x, ..., .list = NULL, .is_node = NULL) {
  stopifnot(is_list(.x))
  y <- dots_list(..., .named = NULL, .homonyms = "error")
  list_recurse(.x, y, function(x, y) y, recurse = FALSE, is_node = .is_node)
}

list_modify <- function(.x, ..., .list = NULL, .is_node = NULL) {
  stopifnot(is_list(.x))
  y <- dots_list(..., .named = NULL, .homonyms = "error")
  list_recurse(.x, y, function(x, y) y, is_node = .is_node)
}

list_merge <- function(.x, ..., .list = NULL, .is_node = NULL) {
  stopifnot(is_list(.x))
  y <- dots_list(..., .named = NULL, .homonyms = "error")
  list_recurse(.x, y, c, is_node = .is_node)
}


list_recurse <- function(x, y, base_f, recurse = TRUE, is_node = NULL) {
  if (length(names(y))) {
    out <- all(names(y) != "" & !is.na(names(y)))
    stopifnot("`...` arguments must be either all named or all unnamed." = out)
  }
  is_node <- as_predicate(is_node %||% vec_is_list)
  idx <- names(y) %||% rev(seq_along(y))
  for (i in idx) {
    x_i <- pluck(x, i)
    y_i <- pluck(y, i)

    if (inherits(y_i, "rlang_zap")) {
      x[[i]] <- NULL
    } else if (recurse && is_node(x_i) && is_node(y_i)) {
      list_slice2(x, i) <- list_recurse(x_i, y_i, base_f)
    } else {
      list_slice2(x, i) <- base_f(x_i, y_i)
    }
  }
  x
}
