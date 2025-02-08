#' Does a list contain an object?
#' @export
has_element <- function(.x, .y) {
  some(.x, identical, .y)
}
