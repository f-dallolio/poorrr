#' Does a list contain an object?
#' @name has_element
NULL

#' @rdname has_element
#' @export
has_element <- function(.x, .y) {
  some(.x, identical, .y)
}
