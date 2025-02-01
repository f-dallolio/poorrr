#' Map over multiple input simultaneously (in "parallel")
#' @name pmap
NULL

#' @rdname pmap
#' @export
pmap <- function(.l, .f, ...) {
    .f <- .as_function(.f)
    args <- .purrr_args_recycle(.l)
    do.call("mapply", c(FUN = list(quote(.f)), args, MoreArgs = quote(list(...)),
        SIMPLIFY = FALSE, USE.NAMES = FALSE))
}
#' @rdname pmap
#' @export
pwalk <- function(.l, .f, ...) {
  pmap(.l = .l, .f = .f, ...)
  invisible(.l)
}

#' @rdname pmap
#' @export
pmap_lgl <- function(.l, .f, ...) {
  as.vector(pmap(.l = .l, .f = .f, ...), "logical")
}
#' @rdname pmap
#' @export
pmap_int <- function(.l, .f, ...) {
  as.vector(pmap(.l = .l, .f = .f, ...), "integer")
}
#' @rdname pmap
#' @export
pmap_dbl <- function(.l, .f, ...) {
  as.vector(pmap(.l = .l, .f = .f, ...), "double")
}
#' @rdname pmap
#' @export
pmap_chr <- function(.l, .f, ...) {
  as.vector(pmap(.l = .l, .f = .f, ...), "character")
}

.purrr_args_recycle <- function(args) {
    lengths <- map_int(args, length)
    n <- max(lengths)
    stopifnot(all(lengths == 1L | lengths == n))
    to_recycle <- lengths == 1L
    args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x,
        n))
    args
}
