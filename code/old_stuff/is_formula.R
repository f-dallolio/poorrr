# #' @rdname XXX
#' @export
is_formula <- function(x, lhs = NULL, ...) {
  if (!is.call(x) && deparse(x[[1]] == "~")) {
    return(FALSE)
  }
  if (is.null(lhs)) {
    return(TRUE)
  }
  identical(length(x) > 2, lhs)
}

# #' @rdname XXX
#' @export
is_bare_formula <- function(x, scoped = TRUE, lhs = NULL) {
  if (!is_formula(x, lhs = lhs)) {
    return(FALSE)
  }
  if (is_null(scoped)) {
    exp_class <- c("call", "formula")
  } else if (is_true(scoped)) {
    exp_class <- "formula"
  } else if (is_false(scoped)) {
    exp_class <- "call"
  } else {
    stop("Argument `scoped` must be `NULL` or a logical value.")
  }
  is_string(class(x), exp_class)
}
