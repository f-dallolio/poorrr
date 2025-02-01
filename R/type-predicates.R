# #'@rdname XXX
#' @export
is_atomic <- function(x, n = NULL) {
  if (!is.atomic(x) || is.null(x)) {
    return(FALSE)
  }
  if (is.null(n)) {
    return(TRUE)
  }
  length(x) %in% n
}

# #'@rdname XXX
#' @export
is_character <- function(x, n = NULL) {
  if (typeof(x) != "character") {
    return(FALSE)
  }
  if (is.null(n)) {
    return(TRUE)
  }
  length(x) %in% n
}

# #'@rdname XXX
#' @export
is_double <- function(x, n = NULL, finite = NULL) {
  if (typeof(x) != "double") {
    return(FALSE)
  }
  if (is.null(n) && is.null(finite)) {
    return(TRUE)
  }
  if (is.null(finite)) {
    return(length(x) %in% n)
  }
  all(is.finite(x)) == finite
}

# #'@rdname XXX
#' @export
is_integer <- function(x, n = NULL) {
  if (typeof(x) != "integer") {
    return(FALSE)
  }
  if (is.null(n)) {
    return(TRUE)
  }
  length(x) %in% n
}

# #'@rdname XXX
#' @export
is_numeric <- function(x, n = NULL) {
  is_integer(x, n) || is_double(x, n)
}

# #'@rdname XXX
#' @export
is_list <- function(x, n = NULL) {
  if (typeof(x) != "list") {
    return(FALSE)
  }
  if (is.null(n)) {
    return(TRUE)
  }
  length(x) %in% n
}

# #'@rdname XXX
#' @export
is_logical <- function(x, n = NULL) {
  if (typeof(x) != "logical") {
    return(FALSE)
  }
  if (is.null(n)) {
    return(TRUE)
  }
  length(x) %in% n
}

# #'@rdname XXX
#' @export
is_vector <- function(x, n = NULL) {
  if (!is.atomic(x) && !is_list(x)) {
    return(FALSE)
  }
  if (is.null(n)) {
    return(TRUE)
  }
  length(x) %in% n
}

