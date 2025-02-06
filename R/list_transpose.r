#' Transpose a list
#' @name list_transpose
NULL

#' @rdname list_transpose
#' @export
list_transpose <- function(.l) {
  if (!length(.l)) {
    return(.l)
  }
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
    .l <- map(.l, function(x) {
      if (is.null(names(x))) {
        set_names(x, inner_names)
      } else {
        x
      }
    })
  }
  .l <- map(.l, as.list)
  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}
