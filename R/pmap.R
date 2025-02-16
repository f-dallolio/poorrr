#' Map over multiple input simultaneously (in "parallel")
#' @name pmap
NULL

#' @rdname pmap
#' @export
pmap <- function(.l, .f, ...) {
  .f <- as.function(.f)
  args <- .rlang_purrr_args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}
#' @rdname pmap
#' @export
pwalk <- function(.l, .f, ...) {
  pmap(.l, .f, ...)
  invisible(.l)
}
#' @rdname pmap
#' @export
pmap_lgl <- function(.l, .f, ...) {
  as.vector(pmap(.l, .f, ...), "logical")
}
#' @rdname pmap
#' @export
pmap_int <- function(.l, .f, ...) {
  as.vector(pmap(.l, .f, ...), "integer")
}
#' @rdname pmap
#' @export
pmap_dbl <- function(.l, .f, ...) {
  as.vector(pmap(.l, .f, ...), "double")
}
#' @rdname pmap
#' @export
pmap_chr <- function(.l, .f, ...) {
  as.vector(pmap(.l, .f, ...), "character")
}
#' @rdname pmap
#' @export
pmap_vec <- function(.l, .f, ...) {
  out <- pmap(.l, .f, ...)
  list_simplify(out, ptype = .ptype)
}

.rlang_purrr_args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}
