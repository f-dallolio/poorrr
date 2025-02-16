#' Keep/discard elements based on their name/position
#' @name keep_at
NULL

#' @rdname keep_at
#' @export
keep_at <- function(x, at) {
  where <- where_at(x, at)
  x[where]
}

#' @rdname keep_at
#' @export
discard_at <- function(x, at) {
  where <- where_at(x, at)
  x[!where]
}



