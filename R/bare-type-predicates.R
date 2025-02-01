# #' @rdname XXX
#' @export
is_bare_atomic <- function(x, n = NULL) {
    !is.object(x) && is_atomic(x, n)
}

# #' @rdname XXX
#' @export
is_bare_character <- function(x, n = NULL) {
    !is.object(x) && is_character(x, n)
}

# #' @rdname XXX
#' @export
is_bare_double <- function(x, n = NULL) {
    !is.object(x) && is_double(x, n)
}

# #' @rdname XXX
#' @export
is_bare_integer <- function(x, n = NULL) {
    !is.object(x) && is_integer(x, n)
}

# #' @rdname XXX
#' @export
is_bare_list <- function(x, n = NULL) {
    !is.object(x) && is_list(x, n)
}

# #' @rdname XXX
#' @export
is_bare_logical <- function(x, n = NULL) {
    !is.object(x) && is_logical(x, n)
}

# #' @rdname XXX
#' @export
is_bare_numeric <- function(x, n = NULL) {
    if (!is_null(n) && length(x) != n) 
        return(FALSE)
    !is.object(x) && typeof(x) %in% c("double", "integer")
}

# #' @rdname XXX
#' @export
is_bare_vector <- function(x, n = NULL) {
    is_bare_atomic(x) || is_bare_list(x, n)
}

