#' Generate or handle a missing argument
#' @export
missing_arg <- function() {
  quote(expr = )
}
#' @export
is_missing <- function(x) {
  missing(x) || identical(x, quote(expr = ))
}
#' @export
maybe_missing <- function(x, default = missing_arg()) {
  if (is_missing(x)) {
    return(default)
  }
  x
}


list_append <- function(.x, .val, .where = NULL) {
  lengx <- length(.x)
  if (is.null(.where)) {
    .where <- lengx
  } else if (is_string(.where)) {
    nms <- names(.x)
    if (length(nms) && .where %in% nms) {
      .where <- which(nms == .where)[[1]]
    } else {
      warning("Reverting to `.before = ", lengx, "`. Cannot find the name ", .where, ".")
      .where <- lengx
    }
  }

  if (!.where) {
    c(.val, .x)
  } else if (.where >= lengx) {
    c(.x, .val)
  } else {
    c(.x[1L:.where], .val, .x[(.where + 1L):lengx])
  }
}
list_prepend <- function(.val, .x, .where = NULL) {
  if (is_string(.where)) {
    nms <- names(.x)
    if (length(nms) && .where %in% nms) {
      .where <- which(nms == .where)[[1]]
    } else {
      warning("Reverting to `.where = NULL`. Cannot find the name ", .where, ".")
      .where <- NULL
    }
  }
  if (!is.null(.where)) {
    .where <- .where - 1
  } else {
    .where <- 0
  }
  list_append(.x, .val, .where = .where)
  #
  # if (is.null(.where)) {
  #   .where <- 1L
  # } else if (is_string(.where)) {
  #   nms <- names(.x)
  #   if (length(nms) && .where %in% nms) {
  #     .where <- which(nms == .where)[[1]]
  #   } else {
  #     warning("Reverting to `.where = ", lengx,"`. Cannot find the name ", .where,".")
  #     .where <- 1L
  #   }
  # } else {
  #   stopifnot(is,numeric(.where))
  # }
  # lengx <- length(.x)
  # if (!.where || .where == 1L) {
  #   c(.val, x)
  # } else if (.where >= lengx) {
  #   c(.x, .val)
  # } else {
  #   c(.x[1L:(.where - 1L)], .val, .x[(.where):lengx])
  # }
}
