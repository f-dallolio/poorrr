#' Utilities: Type predicates
#' @name utils-type-predicates
NULL

#' @rdname utils-type-predicates
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

#' @rdname utils-type-predicates
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

#' @rdname utils-type-predicates
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

#' @rdname utils-type-predicates
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

#' @rdname utils-type-predicates
#' @export
is_numeric <- function(x, n = NULL) {
  is_integer(x, n) || is_double(x, n)
}

#' @rdname utils-type-predicates
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

#' @rdname utils-type-predicates
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

#' @rdname utils-type-predicates
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

#' Utilities: Type predicates (scalar)
#' @name utils-scalar-predicates
NULL

#' @rdname utils-scalar-predicates
#' @export
is_scalar_numeric <- function(x) {
  is_integer(x, n = 1) || is_double(x, n = 1)
}

#' @rdname utils-scalar-predicates
#' @export
is_scalar_list <- function(x) {
  is_list(x, n = 1)
}

#' @rdname utils-scalar-predicates
#' @export
is_scalar_logical <- function(x) {
  is_logical(x, n = 1)
}

#' @rdname utils-scalar-predicates
#' @export
is_scalar_vector <- function(x) {
  is_vector(x, n = 1)
}

#' @rdname utils-scalar-predicates
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

#' @rdname utils-scalar-predicates
#' @export
is_bool <- function(x) {
  is_logical(x, n = 1)
}

#' Utilities: Type predicates (bare)
#'
#' @name utils-bare-predicates
NULL

#' @rdname utils-bare-predicates
#' @export
is_bare_atomic <- function(x, n = NULL) {
  !is.object(x) && is_atomic(x, n)
}

#' @rdname utils-bare-predicates
#' @export
is_bare_character <- function(x, n = NULL) {
  !is.object(x) && is_character(x, n)
}

#' @rdname utils-bare-predicates
#' @export
is_bare_double <- function(x, n = NULL) {
  !is.object(x) && is_double(x, n)
}

#' @rdname utils-bare-predicates
#' @export
is_bare_integer <- function(x, n = NULL) {
  !is.object(x) && is_integer(x, n)
}

#' @rdname utils-bare-predicates
#' @export
is_bare_list <- function(x, n = NULL) {
  !is.object(x) && is_list(x, n)
}

#' @rdname utils-bare-predicates
#' @export
is_bare_logical <- function(x, n = NULL) {
  !is.object(x) && is_logical(x, n)
}

#' @rdname utils-bare-predicates
#' @export
is_bare_numeric <- function(x, n = NULL) {
  if (!is_null(n) && length(x) != n)
    return(FALSE)
  !is.object(x) && typeof(x) %in% c("double", "integer")
}

#' @rdname utils-bare-predicates
#' @export
is_bare_vector <- function(x, n = NULL) {
  is_bare_atomic(x) || is_bare_list(x, n)
}

