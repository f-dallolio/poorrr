#' Safely get or set an element deep within a nested data structure
#' @name pluck
NULL

#' @rdname pluck
#' @export
`pluck<-` <- function(.x, ..., value) {
  assign_in(.x, list2(...), value)
}

#' @rdname pluck
#' @export
pluck <- function(.x, ...) {
  .i <- list2(...)
  pluck_raw(.x = .x, .i = .i, .default = .default)
}

#' @rdname pluck
#' @export
pluck_exists <- function(.x, ...) {
  .i <- list2(...)
  nms <- names(.i)
  stopifnot(is.null(nms) || all(nms == ""))
  !is_zap(pluck_raw(.x, .i, .default = zap()))
}



