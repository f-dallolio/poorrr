#' Simplify a list to an atomic or S3 vector
#' @export
list_simplify <- function (x, ..., strict = TRUE) {
  stopifnot(...length() == 0)
  stopifnot(is_bool(strict))
  if (strict) {
    stopifnot(all(vapply(x, is.vector, logical(1))))
    stopifnot(all(lengths(x) == 1))
  } else {
    can_simplify <- all(vapply(x, is.vector, logical(1))) && all(lengths(x) == 1)
    if (!can_simplify) {
      return(x)
    }
  }
  names <- names(x)
  x <- set_names(x, NULL)
  out <- do.call(c, x)
  names(out) <- names
  out
}





