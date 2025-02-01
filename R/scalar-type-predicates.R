# #' @rdname XXX
#' @export
is_scalar_atomic <- function(x) {
    is_atomic(x, n = 1)
}

# #' @rdname XXX
#' @export
is_scalar_character <- function(x) {
    is_character(x, n = 1)
}

# #' @rdname XXX
#' @export
is_scalar_double <- function(x) {
    is_double(x, n = 1)
}

# #' @rdname XXX
#' @export
is_scalar_integer <- function(x) {
    is_integer(x, n = 1)
}

# #' @rdname XXX
#' @export
is_scalar_numeric <- function(x) {
  is_integer(x, n = 1) || is_double(x, n = 1)
}

# #' @rdname XXX
#' @export
is_scalar_list <- function(x) {
    is_list(x, n = 1)
}

# #' @rdname XXX
#' @export
is_scalar_logical <- function(x) {
    is_logical(x, n = 1)
}

# #' @rdname XXX
#' @export
is_scalar_vector <- function(x) {
    is_vector(x, n = 1)
}

# #'@rdname XXX
#' @export
is_string <- function(x, string = NULL) {
  if (!is_character(x, n = 1)) {
    return(FALSE)
  }
  if (is.null(string)) {
    return(TRUE)
  }
  x %in% string
}

# #'@rdname XXX
#' @export
is_bool <- function(x) {
  is_logical(x, n = 1)
}
