#' Standalone `purrr`-like functions
#'
#' @name purrr-like
NULL
#' @rdname purrr-like
#' @export
 pmap <- function(.l, .f, ...) {
    .f <- as.function(.f)
    args <- .args_recycle(.l)
    do.call("mapply", c(FUN = list(quote(.f)), args, MoreArgs = quote(list(...)),
        SIMPLIFY = FALSE, USE.NAMES = FALSE))
 }
 #' @rdname purrr-like
 #' @export
 pwalk <- function(.l, .f, ...) {
   pmap(.l, .f, ...)
   invisible(.l)
 }
 #' @rdname purrr-like
 #' @export
 pmap_lgl <- function(.l, .f, ...) {
   as.vector(pmap(.l, .f, ...), "logical")
 }
 #' @rdname purrr-like
 #' @export
 pmap_int <- function(.l, .f, ...) {
   as.vector(pmap(.l, .f, ...), "integer")
 }
 #' @rdname purrr-like
 #' @export
 pmap_dbl <- function(.l, .f, ...) {
   as.vector(pmap(.l, .f, ...), "double")
 }
 #' @rdname purrr-like
 #' @export
 pmap_chr <- function(.l, .f, ...) {
   as.vector(pmap(.l, .f, ...), "character")
 }
.args_recycle <- function(args) {
    lengths <- vapply(args, length, integer(1))
    n <- max(lengths)
    stopifnot(all(lengths == 1L | lengths == n))
    to_recycle <- lengths == 1L
    args[to_recycle] <- lapply(args[to_recycle], rep, n)
    args
}
#' @rdname purrr-like
#' @export
 keep <- function(.x, .f, ...) {
    .x[where_if(.x, .f, ...)]
}
#' @rdname purrr-like
#' @export
 discard <- function(.x, .p, ...) {
    sel <- where_if(.x, .p, ...)
    .x[is.na(sel) | !sel]
}
#' @rdname purrr-like
#' @export
 map_if <- function(.x, .p, .f, ..., .else = NULL) {
    matches <- where_if(.x, .p)
    .x[matches] <- map(.x[matches], .f, ...)
    if (is.null(.else)) return(.x)
    .x[!matches] <- map(.x[!matches], .else)
    .x
}
#' @rdname purrr-like
#' @export
 where_if <- function(.x, .p, ...) {
    if (!is_logical(.p)) {
      .p <- map_lgl(.x, as_function(.p, env = global_env()), ...)
    } else {
      stopifnot(length(.p) == length(.x))
    }
   unname(.p)
}
#' @rdname purrr-like
#' @export
 map_at <- function(.x, .at, .f, ...) {
    map(.x[where_at(.x, .at)], .f, ...)
 }

 vec_as_location <- function (i, n, names = NULL) {
   out <- seq_len(n)
   names(out) <- names
   if (is_logical(i, n = 1) && !length(out)) {
     return(out)
   }
   unname(out[i])
 }
#' @rdname purrr-like
#' @export
 where_at <- function(.x, .at, ...) {
    if (is.call(.at) && call_name(.at))
        .at <- as_function(.at)
    if (is.function(.at))
        .at <- .at(names2(x), ...)
    if (is.numeric(.at) || is.logical(.at) || is.character(.at)) {
        if (is.character(.at))
            .at <- intersect(.at, names2(x))
        loc <- vec_as_location(.at, length(.x), names2(.x))
        seq_along(.x) %in% loc
    }
    else {
        stop("`at` must be a numeric vector, character vector, or function.")
    }
}
#' @rdname purrr-like
#' @export
 compact <- function(.x) {
    Filter(length, .x)
}
#' @rdname purrr-like
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
            }
            else {
                x
            }
        })
    }
    .l <- map(.l, as.list)
    map(fields, function(i) {
        map(.l, .subset2, i)
    })
}
#' @rdname purrr-like
#' @export
 every <- function(.x, .p, ...) {
    .p <- as_function(.p, env = global_env())
    for (i in seq_along(.x)) {
        if (is_false(.p(.x[[i]], ...)))
            return(FALSE)
    }
    TRUE
}
#' @rdname purrr-like
#' @export
 some <- function(.x, .p, ...) {
    .p <- as_function(.p, env = global_env())
    for (i in seq_along(.x)) {
        if (is_true(.p(.x[[i]], ...)))
            return(TRUE)
    }
    FALSE
}
#' @rdname purrr-like
#' @export
 none <- function(.x, .p, ...) {
    !some(.x, .p, ...)
}
#' @rdname purrr-like
#' @export
 negate <- function(.p) {
    .p <- as_function(.p, env = global_env())
    function(...) !.p(...)
}
#' @rdname purrr-like
#' @export
 reduce <- function(.x, .f, ..., .init) {
    f <- function(x, y) as_function(.f)(x, y, ...)
    Reduce(f, .x, init = .init)
}
#' @rdname purrr-like
#' @export
 reduce_right <- function(.x, .f, ..., .init) {
    f <- function(x, y) as_function(.f)(y, x, ...)
    Reduce(f, .x, init = .init, right = TRUE)
}
#' @rdname purrr-like
#' @export
 accumulate <- function(.x, .f, ..., .init) {
    f <- function(x, y) as_function(.f)(x, y, ...)
    Reduce(f, .x, init = .init, accumulate = TRUE)
}
#' @rdname purrr-like
#' @export
 accumulate_right <- function(.x, .f, ..., .init) {
    f <- function(x, y) as_function(.f)(y, x, ...)
    Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}
#' @rdname purrr-like
#' @export
 detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
    .p <- as_function(.p, env = global_env())
    .f <- as_function(.f, env = global_env())
    for (i in .index(.x, .right)) {
        if (.p(.f(.x[[i]], ...))) {
            return(.x[[i]])
        }
    }
    NULL
}
#' @rdname purrr-like
#' @export
 detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
    .p <- as_function(.p, env = global_env())
    .f <- as_function(.f, env = global_env())
    for (i in .index(.x, .right)) {
        if (.p(.f(.x[[i]], ...))) {
            return(i)
        }
    }
    0L
}
.index <- function(x, right = FALSE) {
    idx <- seq_along(x)
    if (right) {
        idx <- rev(idx)
    }
    idx
}
#' @rdname purrr-like
#' @export
 list_c <- function(x) {
    do.call(c, x)
}
list_c(as.list(letters))
