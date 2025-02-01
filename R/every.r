#' Do every, some, or none of the elements of a list satisfy a predicate?
#' @name every
NULL

#' @rdname every
#' @export
every <- function(.x, .p, ...) {
    .p <- .as_function(.p, env = globalenv())
    for (i in seq_along(.x)) {
        if (!is_true(.p(.x[[i]], ...))) {
            return(FALSE)
        }
    }
    TRUE
}
#' @rdname every
#' @export
some <- function(.x, .p, ...) {
    .p <- .as_function(.p, env = globalenv())
    for (i in seq_along(.x)) {
        if (is_true(.p(.x[[i]], ...))) {
            return(TRUE)
        }
    }
    FALSE
}
#' @rdname every
#' @export
none <- function(.x, .p, ...) {
    !some(.x, .p, ...)
}
